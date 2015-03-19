{
   DUnit: An XTreme testing framework for Delphi and Free Pascal programs.

   The contents of this file are subject to the Mozilla Public
   License Version 1.1 (the "License"); you may not use this file
   except in compliance with the License. You may obtain a copy of
   the License at http://www.mozilla.org/MPL/

   Software distributed under the License is distributed on an "AS
   IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
   implied. See the License for the specific language governing
   rights and limitations under the License.

   The Original Code is DUnit.

   The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
   and Juancarlo Añez.
   Portions created The Initial Developers are Copyright (C) 1999-2000.
   Portions created by The DUnit Group are Copyright (C) 2000-2007.
   All rights reserved.

   Contributor(s):
   Kent Beck <kentbeck@csi.com>
   Erich Gamma <Erich_Gamma@oti.com>
   Juanco Añez <juanco@users.sourceforge.net>
   Chris Morris <chrismo@users.sourceforge.net>
   Jeff Moore <JeffMoore@users.sourceforge.net>
   Uberto Barbini <uberto@usa.net>
   Brett Shearer <BrettShearer@users.sourceforge.net>
   Kris Golko <neuromancer@users.sourceforge.net>
   The DUnit group at SourceForge <http://dunit.sourceforge.net>
   Peter McNab <mcnabp@gmail.com>
   Graeme Geldenhuys <graemeg@gmail.com>
}

{$IFNDEF SELFTEST}
  '!!!Alert SELFTEST must be defined in project options conditionals'
{$ENDIF}

unit UnitTestFramework;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ELSE}
  // If Delphi 7, turn off UNSAFE_* Warnings
  {$IFNDEF VER130}
    {$IFNDEF VER140}
      {$WARN UNSAFE_CODE OFF}
      {$WARN UNSAFE_CAST OFF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface

uses
  ProjectsManagerIface,
  TestFrameworkIfaces,
  {$IFDEF SELFTEST}
    RefTestFramework,
  {$ELSE}
    TestFramework,
  {$ENDIF}
  SysUtils,
  Classes;

type
{------------------------------------------------------------------------------}
  {ITestProc section }

  TTestITestProc = class(TTestCase)
  private
    FTestProc: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSet_Enabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestGet_Enabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSet_Excluded;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestGet_Excluded;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSupportedType;
  end;

  TTestNamedTestCase = class(TTestCase)
  private
    FTestCase: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreatesWithNoString;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreatesWithBlankString;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreatesWithString;
  end;

  TTestITestProcName = class(TTestCase)
  private
    FTestProc: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDisplayedNameEmpty;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDisplayedName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestGetNameReturnsDisplayedName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestParentPathEmpty;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestParentPath;
  end;

  TTestITestProcCreatesNamed = class(TTestCase)
  private
    FTestProc: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestProcCreatesEmptyNamed;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestProcCreatesNamed;
  end;

{------------------------------------------------------------------------------}
  { ITestCase section }

  TTestITestCaseExists = class(TTestCase)
  private
    FTestCase: ITest;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreates;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDestroys;
  end;

  TExceptTestCase = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ATestExcepts;
  end;

  TTestTestCase1 = class(TTestCase)
  private
    FTestCase1: ITestCase;
    FExceptOnRun: TExceptTestCase;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestExceptionInRun;
  end;

  ITestCase1 = interface(ITestCase)
  ['{B51B9D8A-0711-41CD-A06D-3E2CCA984882}']
    function get_LocalLevel: Integer;
  end;

  TTestDecoy = class(TTestCase)
    procedure DecoyTest;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ATestProcedure;
  end;

  TTestNonPublished = class(TTestCase)
  private
    FTestCase: TTestDecoy;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNonPublishedProcedure;
  end;

  TTestCase1 = class(TTestCase, ITestCase1)
  private
    FLocalLevel: Integer;
    function get_LocalLevel: Integer;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ATestProcedure;
  end;

  TTestParentPathPopulatesTests = class(TTestCase)
  private
    FTestCase1: ITestCase1;

  protected
    procedure SetUp;    override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestParentPathSet;
  end;

  TTestLevelTestRunsFrom = class(TTestCase)
  private
    FTestCase1: ITestCase1;
    FAnExecControl: ITestExecControl;
    FTestProject: ITestProject;

  protected
    procedure SetUp;    override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestMethodDepths;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestATestProcLevel;
  end;

  ITestTestCase0 = interface(ITestCase)
  ['{545D434D-F634-4990-AE06-0D2436343AE3}']
    function  Get_SetUpRan: boolean;
    function  Get_RunRan: boolean;
    function  Get_TearDownRan: boolean;
  end;

  ITestOnceFunctions = interface(ITestCase)
  ['{26606884-1C7A-4DC8-B5BA-648DC3EB328A}']
    procedure InitBools;
    function  Get_SetUpOnceRan: boolean;
    function  Get_TearDownOnceRan: boolean;
  end;

  // TTestATTestCase creates a TTestCase and calls all of it's advertised
  // procedures to ensure they are all called.

  ITestTestCase2 = interface(ITestTestCase0)
  ['{869BB924-D2E7-4497-9A54-FFDCEE790F1E}']
    function  Get_ATestProcRan: boolean;
  end;

  TTestCase2 = class(TTestCase, ITestTestCase2)
  private
    FSetUpRan: boolean;
    FTearDownRan: boolean;
    FRunRan: boolean;
    FTestProcRan: boolean;
    function  Get_SetUpRan: boolean;
    function  Get_TearDownRan: boolean;
    function  Get_RunRan: boolean;
    function  Get_ATestProcRan: boolean;
  protected
    procedure SetUp; override;
    function  Run(const ExecControl: ITestExecControl): TExecutionStatus; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ATestProcedure;
  end;

  TTestATTestCase = class(TTestCase)
  private
    FTestCase2: ITestTestCase2;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreateDestroyLeaks;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSetUpRan;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTearDownRan;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRunRan;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestATestProcRan;
  end;

  // TTest2TTestCase creates a TTestCase and calls it's run method which
  // iterates over the published procedures and check that all are called.

  ITestTestCase3 = interface(ITestCase)
  ['{909E60CF-D8B9-4EF7-B388-A84E5C98EBC4}']
    function  Get_TestProcRan: Integer;
  end;

  TTestCase3 = class(TTestCase, ITestTestCase3)
  private
    FTestProcRan: Integer;
    function  Get_TestProcRan: Integer;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcedure1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcedure2;
  end;

  TTest2TTestCase = class(TTestCase)
  private
    FTestCase3: ITestTestCase3;
    FAnExecControl: ITestExecControl;
    FCanRun: boolean;
    function ShouldRunTest(const ATest: ITest): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestProcsRan;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestGetsNextEnabledTest;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestOnlyRunsEnabledTest;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestOnlyRunsSingleEnabledTest;
  end;

  TChecksGetCurrentTestCasePath = class(TTestCase)
  protected
    function GetName: string; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ChecksGetName;
  end;

  TVerifyOverriddenGetNameIsAccessed = class(TTestCase)
  private
    FTestCase: ITestCase;
    FAnExecControl: ITestExecControl;
    FInCallbackGetsExecutedOverriddenProcName: string;
    FInCallbackGetsThisOverriddenProcName: string;
    FInCallbackGetsProcName: string;
    FInCallbackGetsThisTestCaseName: string;
    FInCallbackGetsExecutedTestCaseName: string;
    FInCallbackGetsThisInheritedTestCaseName: string;
    FCallbackProcDepth: Integer;
    FTestProcDepth: Integer;
    procedure ExecStatusCallBack(const ATest: ITest);
  protected
    function  GetName: string; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyCallbackTestCaseName;
  end;


  TExecutableTestCase = class(TTestCase)
  protected
    function GetName: string; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Passes;
  end;

  TVerifyGetNameInSuiteIsAccessed = class(TTestCase)
  private
    FTestSuite: ITestSuite;
    FInCallbackGetsExecutedOverriddenProcName: string;
    FInCallbackGetsExecutedTestCaseName: string;
    FCallbackProcDepth: Integer;
    FTestProcDepth: Integer;
    procedure ExecStatusCallBack1(const ATest: ITest);
    procedure ExecStatusCallBack2(const ATest: ITest);
  protected
    function  GetName: string; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyCallbackTestSuiteName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyCallbackTestSuiteOverriddenName;
  end;


  TTestTestCaseCanCountAndRetrieveList = class(TTestCase)
  private
    FTestCase: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyCountReturnsTotalEnabledTestMethodCount;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyPriorTestFetchesTestMethodsInReverseOrder;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyResetResetsTestMethodIterator;
  end;

  TLongModeExitsOnFail = class(TTestCase)
  published
    procedure ShouldExitOnFail;
  end;

  TLongModeNoExitOnPass = class(TTestCase)
  published
    procedure ShouldContinueOnPass;
  end;

  TExitsEarlyOnTrue = class(TTestCase)
  published
    procedure ShouldExitEarly;
  end;

  TNoExitOnFalse = class(TTestCase)
  published
    procedure ShouldNotExitOnFalse;
  end;

  TEmptyCheckExit = class(TTestCase)
  published
    procedure EmptyMethodReportsCHMsgOnFalse;
  end;

  TPassFollowsCheckExit = class(TTestCase)
    procedure MethodHasPassingCheck;
  end;

  TTwoPassesFollowsCheckExit = class(TTestCase)
    procedure MethodHasPassingChecks;
  end;
  { TTestCheckExitBehaviour }

  TTestCheckExitBehaviour = class(TTestCase)
  private
    FXC: ITestExecControl;
    FStatus: TExecutionStatus;
    FStatusMsg: string;
    FTestProject: ITestProject;
    FErrorMsg: string;
    procedure StatusMsgUpdater(const ATest: ITest;
                               const AStatusMsg: string);
    procedure ExecStatusUpdater(const ATest: ITest);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure VerifyLongModeExitsOnFail;
    procedure VerifyLongModeNoExitsOnPass;
    procedure VerifyExitsEarlyOnTrue;
    procedure VerifyNoExitOnFalse;
    procedure VerifyEmptyCEReportsAndFails;
    procedure VerifyFollowingFailReports;
    procedure VerifyReportsAndFailsOnChecksPass;
    procedure VerifySerialCEsAllReportAndFails;
  end;


  // TTest3LevelsCase creates a TTestCase on the fly and calls it's run method
  // which in turn calls each published procedure contained therein.
  // It checks that recursion is functional

  ITestCase4 = interface(ITestCase)
  ['{DECE9568-3E26-418E-B7EC-A08CB1CABAEA}']
    function  Get_TestLevelRan: Integer;
  end;

  TTest3TTestCase = class(TTestCase, ITestCase4)
  private
    FTestProcsRan: Integer;
    FTestCase3: ITestTestCase3;
    FAnExecControl: ITestExecControl;
    function  Get_TestLevelRan: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestProcsRan;
  end;

  TTest3LevelsCase = class(TTestCase)
  private
    FTest3TTestCase: ITestCase4;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestProcsRan;
  end;

  TTestWithRunTestMethod = class(TAbstractTest)
  private
    FHasRun: boolean;
  protected
    procedure RunTest; override;
  public
    function HasRun: boolean;
  end;

  TTestWithRunTestFailedCheck = class(TAbstractTest)
  private
    FHasRun: boolean;
  protected
    procedure RunTest; override;
  public
    function HasRun: boolean;
  end;

  TTestSuiteRunsTestMethod = class(TTestCase)
  public
  end;

  TTestLegacyRunTestMethod = class(TTestCase)
  private
    FSuite: ITestCase;
    FAnExecControl: ITestExecControl;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLegacyAbstractTestRegisters;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRunTestRan;
  end;

  TTestLegacyRunTestMethodCheckFails = class(TTestCase)
  private
    FSuite: ITestCase;
    FAnExecControl: ITestExecControl;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestRunTestFailedCheck;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanDisableRunTest;
  end;

{------------------------------------------------------}
  { ITestSuite section }

  TTestITestSuiteExists = class(TTestCase)
  private
    FTestSuite: ITestSuite;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreates;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreatesEmptyName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreatesHasName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDestroys;
  end;

  TTestITestSuiteCanCountAndList = class(TTestCase)
  private
    FTestSuite: ITestSuite;
    FTestCase : ITestTestCase3;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestSuiteCanCountTestMethods;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyPriorTestFetchesTestMethodsInReverseOrder;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestSuiteCanBeReset;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestSuiteDoesNotCountDisabledTestMethods;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestSuiteDoesNotCountExcludedTestMethods;
  end;

  TTestMethodHasParam = class(TTestCase)
  published
//    {$IFDEF CLR}[Test]{$ENDIF}
    procedure HasParam(const Value: Integer);
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure NormalMethod;
  end;

  TTestRegisrationOfParamMethodTestCase = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    FTestCase : TTestMethodHasParam;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
//    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestCaseDoesNotListParamaterizedTestMethods;
//    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestCaseOnlyRegistersNormalTestMethods;
//    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestCaseOnlyRunsNormalTestMethods;
  end;

  TTestITestSuiteAddMethods = class(TTestCase)
  private
    FTestSuite: ITestSuite;
    FSuite : ITestTestCase3;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNameOnAddSuite;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestGetsNextEnabledTest;
  end;

  TTestTTestITestSuiteAddsTestCases = class(TTestCase)
  private
    FTestSuite: ITestSuite;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteCreatesOnEmptyParams;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteCreatesOnNilTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteCreatesOnEmptyName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteCreatesOnOneTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteCreatesOnMultipleTestCases;
  end;

  TTestITestSuiteRegistersTestCases = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersSingleTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersSingleTestSuite;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersNamedSingleTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersTwoTestSuites;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersTwoTestCases;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersDifferentNamedTwoTestSuites;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTestSuiteRegistersSameNamedTwoTestSuites;
  end;

  TTestTestSuiteParentPathPropagates = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestUnNamedTestSuitePath;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNamedTestSuitePath;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestChangedNamedPropogatesParentPath;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestChangedParentPathPropogatesParentPath;
  end;

{------------------------------------------------------------------------------}
  { ITestProject section }

  TTestITestProjectExists = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCreates;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDestroys;
  end;

  TTestTestProjectBehaviorOnRegisterFirstTestCase = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestProjectCreatedOnRegisterFirstTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestProjectIsNamedOnRegisterFirstTestCase;
  end;

  TTestITestProjectIterator = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TTestIterateOverSingleTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TTestIterateOverSingleRegisteredTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TTestIterateOverTwoTestCases;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TTestIterateOverThreeTestSuites;
  end;

  TTestTestProjectAddMethods = class(TTestCase)
  private
    FTestSuite: ITestSuite;
    FSuite : ITestTestCase3;
    FTestProject: ITestProject;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckCountEnabledTests;
  end;

  TTestTestProjectAddSuite = class(TTestCase)
  private
    FTestSuite: ITestSuite;
    FSuite : ITestTestCase3;
    FTestProject: ITestProject;
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNameOnAddSuite;
  end;

  TTestCase4 = class(TTestCase)
  private
    FTestProcRan: Integer;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcedure1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcedure2;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcedure3;
  end;

  TPassAndFail = class(TTestCase)
    published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcPasses;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProcFails;
  end;

  TStatusRecord = record
                    ExecStatus: TExecutionStatus;
                    AName: string;
                  end;

  TTestITestCanRunProject = class(TTestCase)
  private
    FTestProject: ITestProject;
    FStatusArray: array of TStatusRecord;
    FSecondPass: boolean;
    FReadyCount: Integer;
    FTest: ITest;
    FList: IInterfaceList;
    FAnExecControl: ITestExecControl;
    function  ShouldRunTest(const ATest: ITest): boolean;
    procedure ExecStatusCallBack1(const ATest: ITest);
    procedure ExecStatusCallBack2(const ATest: ITest);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanRunOneTestProc;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanIndividuallyRunOneTestProc;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanRunTwoTestProcs;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanRunTwoSuites;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestMassClearStatus;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestStatusCallBack;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPMLevelSingleProjectStatusCallBack;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPMLevelMultiProjectStatusCallBack;
  end;

  TTestMultiProjectCount = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    FReadyCount: Integer;
    FTest: ITest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPMLevelSingleProjectCounts;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPMLevelMultiProjectCounts;
  end;

{------------------------------------------------------}
  TTestCallToFail = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCallFail;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCallFailEquals;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCallFailNotEquals;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCallFailNotSame;
  end;

{ TTestITestHandlesFailCalls }

  TTestTestCaseHandlesFailCalls = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestCaseFailCalls;
  end;

{------------------------------------------------------}
  TTestCallToHalt = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPass;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestHalts;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestFails;
  end;

  TTestTestCaseHandlesHaltCall = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestCaseHalts;
  end;

{------------------------------------------------------}
{$IFNDEF CLR}
  TTestEmptyTest = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestEmptyTest;
  end;

  TTestFailsOnOptimizedEmptyTest = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestITestEmptyTest;
  end;
{$ENDIF}

{------------------------------------------------------}
  TTestCheckNotCalled = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIsCalled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotCalled;
  end;

  TTestFailsOnCheckNotCalled = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestCheckNotCalled;
  end;

{------------------------------------------------------}
  TTestUnknownExceptHandled = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestUnKnownExcept;
  end;

  TTestUnExpectedExceptHandled = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestUnExpectedExcept;
  end;

  TTestExpectedExceptHandled = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestExpectedExcept;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestExpectedExceptClears;
  end;

  TTestMissingExpectedExcept = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestMissingExpectedExcept;
  end;

  TTestAssertMessageHandled = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestAssert;
  end;

  TTestExceptionHandled = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestHandlesUnknowExcept;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestHandlesUnExpectedExcept;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestHandlesAssert;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestHandlesExpectedExcept;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestHandlesMissingExpectedExcept;
  end;

{------------------------------------------------------}
  TTestExceptInDoSetup = class(TTestCase)
  protected
    procedure SetUpOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestExceptInDoSetUp;
  end;

  TTestExceptInSetup = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Test1stMethodWithSetUpFailure;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Test2ndMethodWithSetUpFailure;
  end;

  TTestExceptInTearDown = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Test1stMethodWithTearDownFailure;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Test2ndMethodWithTearDownFailure;
  end;

  TTestExceptInDoTearDown = class(TTestCase)
  protected
    procedure TearDownOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure DoTestExceptInDoTearDown;
  end;

  THasSetupFailureOn1stMethod = class(TTestCase)
  private
    FCount: Integer;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure SetUpFailsandMethod1FailsIfExecuted;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Method2SetUpOKandPasses;
  end;

  THasSetupFailureOn2ndMethod = class(TTestCase)
  private
    FCount: Integer;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Method1SetUpOKandPasses;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure SetUpFailsandMethod2FailsIfExecuted;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Method3SetUpOKandPasses;
  end;

  THasTeardownFailure = class(TTestCase)
  private
    FCount: Integer;
  protected
    procedure SetUpOnce; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ExecutedMethodPasses;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ExecutedMethodTearDownFails;
  end;

  TTestTestCaseHandlesNonMethodFailures  = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckSetupFailure;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckTeardownFailure;
  end;

  IWhatRan = interface
  ['{01DAECE0-FCE4-4C44-AFBF-82342CFD0E2A}']
    function SetUpRan: boolean;
    function MethodRan: boolean;
    function TearDownRan: boolean;
  end;

  TTestExceptInSetupAndTearDown = class(TTestCase, IWhatRan)
  private
    FSetUpRan: boolean;
    FMethodRan: boolean;
    FTearDownRan: boolean;
    function SetUpRan: boolean;
    function MethodRan: boolean;
    function TearDownRan: boolean;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestWithSetUpAndTearDownFailure;
  end;


  // Registered as TTestSuite
  TTestSuiteHasSetUpFailure = class(TTestSuite)
  protected
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure XRuns;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure YRuns;
  end;

  // Registered as TTestSuite
  TTestSuiteHasTearDownFailure = class(TTestSuite)
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure XRuns;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure YRuns;
  end;

  //Test to verify SetUp and Teardown failures at TTestSuite level get handled too
  TTestHandlesTestSuiteNonMethodFailures  = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifySetUpFailureInTTestSuiteHandled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTearDownFailureInTTestSuiteHandled;
  end;

  IMethodRan = interface
  ['{9F37D01A-40AD-4949-BF58-729CD86B57CA}']
    function MethodRan: boolean;
  end;

  TTestSimplePass1 = class(TTestCase, IMethodRan)
  private
    FMethodRan: boolean;
    function MethodRan: boolean;
  protected
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure SimpleMethod1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure SimpleMethodFails;
  end;

  TTestSimplePass2 = class(TTestCase, IMethodRan)
  private
    FMethodRan: boolean;
    function MethodRan: boolean;
  protected
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure SimpleMethod2;
  end;

  TTestTearDownFailInTestSuite = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestTheWholeSuiteRuns;
  end;

  TTestPeripheryExceptions = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    FStatusArray: array of TStatusRecord;
    FTest: ITest;
    FList: IInterfaceList;
    FErrorMsg: string;
    procedure ExecStatusCallBack(const ATest: ITest);
    procedure ExecStatusCallBackMsg(const ATest: ITest);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInDoSetUp;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInSetUp;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInSetUpReports;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInTearDown;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInDoTearDown;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestITestExceptionInSetUpAndTearDown;
  end;

{------------------------------------------------------}
  TTestGens2Errors = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenEAbortError1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenExceptError2;
  end;

  TTestGens2Failures = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenFailError1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenStopError2;
  end;

  TTestGens2Warnings = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenCheckFail;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenCheckWarning1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure GenCheckWarning2;
  end;

{$IFNDEF CLR}
  TTestGens2MemWarning2 = class(TTestCase)
  private
    FObj1, FObj2: Tobject;
  protected
    procedure TearDownOnce; override;
  published
    procedure GenMemWarning1;
    procedure GenMemWarning2;
  end;
{$ENDIF}

  TTestErrorReporting = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestErrorsReport;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestFailuresReport;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckWarningsReport;
    {$IFNDEF CLR}
    procedure TestMemWarningsReport;
    {$ENDIF}
  end;

  TTestWith3Procs = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Proc1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Proc2;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Proc3;
  end;

  TTestExcludedTestReporting = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    function IsTestExcluded(const ATest: ITest): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyExcludedTestGetReported;
  end;

  TTestHandlesBreakOnNotPassed = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestsBreakOnFailure;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestsBreakOnException;
  end;

{------------------------------------------------------}
  TTestFullTestCaseCalled = class(TTestCase)
  private
    FSetupOnceCalled: boolean;
    FSetupCalled: boolean;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestExecutesNamedMethodSetup;
  end;

{------------------------------------------------------}
  TTestCreateNamedTest = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanEnable1stOfMultipleMethods;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanEnable2ndOfMultipleMethods;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCanExecuteNamedMethodSetup;
  end;

{------------- Used in type checking tests ------------}
  TTypeA = class(TObject)
  end;

  TTypeB = class(TObject)
  end;

  TTypeAAofANotB = class(TTypeA)
  end;

  TTypeBBofBNotA = class(TTypeB)
  end;

  TTestCheckInherits = class(TTestCase)
  private
    Obj1: TObject;
  protected
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ExpectedIsNil;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ActualIsNil;
  end;

  TTestCheckIs = class(TTestCase)
  private
    Obj1: TObject;
  protected
    procedure TearDown; override;
  public
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ExpectedIsNil;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ActualIsNil;
  end;

{------------------------------------------------------}
  TObjectA = class(TObject);
  TObjectB = class(TObject);

  TTestChecks = class(TTestCase)
  private
    FAnExecControl: ITestExecControl;
    s1, s2: WideString;
    Obj1, Obj2: Tobject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure RaiseAgreedException;
    procedure RaiseDifferentException;
    procedure DontRaiseException;
    destructor Destroy; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure RunOldTestCheck;   //24 common and 3 more win32 only
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckTrueP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckTrueF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckTrueMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckTrueMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckFalseP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckFalseF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckFalseMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckFalseMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanCheckEqualsP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanCheckEqualsF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanCheckEqualsMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanCheckEqualsMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanNotCheckEqualsP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanNotCheckEqualsF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanNotCheckEqualsMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestBooleanNotCheckEqualsMsgF;

    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsIntP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsIntF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsIntMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsIntMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsIntP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsIntF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsIntMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsIntMsgF;

    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsInt64P;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsInt64F;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsInt64MsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsInt64MsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsInt64P;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsInt64F;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsInt64MsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsInt64MsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndDeltaP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndDeltaF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndDeltaP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndDeltaF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndDeltaMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsExtndDeltaMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndDeltaMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsExtndDeltaMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStrP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStrF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStrP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStrF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStrMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStrMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStringP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStringF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStringP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStringF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStringMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsStringMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStringMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsStringMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsBinP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsBinF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsBinP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsBinF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsHexP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckEqualsHexF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsHexP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotEqualsHexF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotNullP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotNullF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNullP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNullF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckExceptionP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckExceptionF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckExceptionN;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameIfaceP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameIfaceF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameIfaceMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameIfaceMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameIfaceP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameIfaceF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameIfaceMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameIfaceMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameObjP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameObjF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameObjMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameObjMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameObjP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameObjF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameObjMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameObjMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameClassP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameClassF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameClassMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckSameClassMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameClassP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameClassF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameClassMsgP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckNotSameClassMsgF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckInheritsP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckInheritsF;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIsP;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestCheckIsF;
    {$IFNDEF CLR}
      {$IFNDEF UNICODE}
       procedure TestCheckEqualsWSP;
       procedure TestCheckEqualsWSF;
       procedure TestCheckNotEqualsWSP;
       procedure TestCheckNotEqualsWSF;
       procedure TestCheckEqualsWSMsgP;
       procedure TestCheckEqualsWSMsgF;
       procedure TestCheckNotEqualsWSMsgP;
       procedure TestCheckNotEqualsWSMsgF;
       procedure TestCheckEqualsMemP;
       procedure TestCheckEqualsMemF;
       procedure TestCheckNotEqualsMemP;
       procedure TestCheckNotEqualsMemF;
       procedure TestCheckEqualsMemMsgP;
       procedure TestCheckEqualsMemMsgF;
       procedure TestCheckNotEqualsMemMsgP;
       procedure TestCheckNotEqualsMemMsgF;
       {$ENDIF}
       procedure TestCheckEqualsWideStringP;
       procedure TestCheckEqualsWideStringF;
       procedure TestCheckEqualsWideStringMsgP;
       procedure TestCheckEqualsWideStringMsgF;
       procedure TestCheckNotEqualsWideStringP;
       procedure TestCheckNotEqualsWideStringF;
    {$ENDIF}
  end;

  type
    TTestCaseType = class of TTestCase;

  TTestCheckTests = class(TTestCase)
  private
    function CheckPassOrFail(const TestName: string;
                             const Outcome: TExecutionStatus): boolean;

    function CheckAllTestsPassOrFail(const TestCaseType: TTestCaseType;
                                     const Outcome: TExecutionStatus;
                                     out   ErrorMsg: string): boolean;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestAllCheckCalls;
  end;

  TTestChecksCount = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestAllChecksCount;
  end;

  TElapsedTimesTests = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Quick;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Sleep20;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure Sleep40;
  end;

  TTestsElapsedTimes = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedSuite;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedSuites;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestElapsedProject;
  end;

  ICheckTestSetUpData = interface(ITestSetUpData)
  ['{8C61F458-86B5-4CD0-BF26-5366D28980CB}']
    function  get_SetupData: string;
    procedure set_SetupData(const Value: string);
    property  SetupData: string read get_SetupData write set_SetupData;
  end;

{$M+}
  TSetupData = class(TInterfacedObject, ICheckTestSetUpData)
  private
    FSetupData: string;
    function  get_SetupData: string;
    procedure set_SetupData(const Value: string);
  published
    property  SetupData: string read get_SetupData write set_SetupData;
  end;
{$M-}

  TSetupDataAccessTest = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckSetupData;
  end;

  TTestSetUpDataAccess = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckSetUpDataPassedDown;
  end;

  TStatusMessageTests = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure AssignStatusMessage1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure AssignStatusMessage2;
  end;

  TTestStatusMessageCallback = class(TTestCase)
  private
    FTestProject: ITestProject;
    FITest: ITest;
    FCalledBack: boolean;
    FCalledBackMessages: TStrings;
    procedure StatusMsgListener(const ATest: ITest;
                                const AStatusMsg: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckCallGetStatusMsg;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckCallBackOnAssignStatusMsg;
  end;

  TTestStatusMessageAutoCallback = class(TTestCase)
  private
    FTestProject: ITestProject;
    FITest: ITest;
    FCalledBack: boolean;
    FCalledBackMessages: TStrings;
    procedure StatusMsgListener(const ATest: ITest;
                                const AStatusMsg: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckAssignStatusMsgCallsback;
  end;

{------- Decorated Testing -------}

  var
    GSetUpOnceCalled: Integer;
    GSetUpCalled: Integer;
    GTearDownCalled: Integer;
    GTearDownOnceCalled: Integer;
    DSetUpOnceCalled: Integer;
    DSetUpCalled: Integer;
    DTearDownCalled: Integer;
    DTearDownOnceCalled: Integer;
    DPass1Called: Integer;
    DPass2Called: Integer;

type
  TDecoratedTest = class(TTestCase)
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPass1;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestPass2;
  end;


// The following sets up shareable data and demonstrates how to pass data from
// a decorator into the decorated test(s)

  IDecoratorSharedData = interface(ITestSetUpData)
  ['{6F573C2E-DD60-47D0-96EF-C69717284586}']

    function  get_RanCount: Integer;
    procedure set_RanCount(const Value: Integer);
    property  RanCount: Integer read get_RanCount write set_RanCount;
  end;

{$M+}
  TDecoratorSharedData = class(TInterfacedObject, IDecoratorSharedData)
  {$IFDEF VER180} strict {$ENDIF}
  private
    FRanCount: Integer;
  protected
    function  get_RanCount: Integer;
    procedure set_RanCount(const Value: Integer);
  published
    property  RanCount: Integer read get_RanCount write set_RanCount;
  end;
{$M-}

// This decorates a set of tests which deliberately fail so the failure reporting
// can be verified
  TVerifyDecoratorHandlesPeripheryFailures = class(TTestDecorator)
  private
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TDecoratedTestWithSetUpOnceFailure = class(TTestCase)
  protected
    procedure SetUpOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ProcPasses;
  end;

  TDecoratedTestWithSetUpFailure = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ProcPasses;
  end;

  TDecoratedTestWithTearDownFailure = class(TTestCase)
  protected
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ProcPasses;
  end;

  TDecoratedTestWithTearDownOnceFailure = class(TTestCase)
  protected
    procedure TearDownOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ProcPasses;
  end;

  TVerifyDecoratorReportsPeripheryFailures = class(TTestCase)
   private
     FProject: ITestProject;
   protected
     procedure SetUp; override;
     procedure TearDown; override;
   published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyDecoratedTestsFlagErrorsInPeripheryProcs;
  end;


  { TTestDecoratesTest }

  TTestDecoratesTest = class(TTestDecorator)
  // Note there is no execute method required;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  end;

  TTestDecoratorDecorates = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckDecoratorPrefixAndNames;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckExecutionSequence;
  end;

  TTestSuiteDecorator = class(TTestDecorator)
  protected
    procedure SetUp; override;
  end;

  TTestDecoratorRegistersTestSuite = class(TTestCase)
  private
    FProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckRegistersTestSuite;
  end;

{-------- Repeated Tests -----------}
var
  GlobalCount: Cardinal;

type
  // Simply override TRepeatedTest
  TTestRepeatedTests = class(TRepeatedTest)
  end;

  TTestCountedTestHalts = class(TTestRepeatedTests)
  protected
    procedure SetUp; override;
  end;

  TCountedTestsWithFailures = class(TTestCase)
  protected
    procedure SetUpOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure PassesA;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ShouldFailOnFirstExecution;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure PassesB;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ShouldFailOnThirdExecution;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure PassesC;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure ShouldFailOnFifthExecution;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure PassesD;
  end;


  TTestRepeatTest = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckFailsAndExecutesNTimes;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckFailsAndHalts;
  end;

{------------------------------------------------------}
  {$IFDEF FASTMM}
  TLeakTestClass = class(TTestCase)
  private
    AnObject: TObject; // something to leak
    procedure ClearVars;
  protected
    procedure TearDownOnce; override;
    procedure SetUp; override;
  end;

  TNoLeaks = class(TLeakTestClass)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RunATest;
  end;

  TLeaksInSetup = class(TLeakTestClass)
  protected
    procedure SetUp; override;
  published
    procedure RunATest;
  end;

  TLeaksInRun = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TLeaksInTearDown = class(TLeakTestClass)
  protected
    procedure TearDown; override;
  published
    procedure RunATest;
  end;

  TTestDefaultIsWarnButIgnoreLeaks = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TTestContainsAllowedTObjectLeak = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TTestContainsAllowedTObjectLeakByList = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TTestContainsAllowedTObjectLeakByEmptyList = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TTestContainsAllowedLeakArrayLongList = class(TLeakTestClass)
  published
    procedure RunATest;
  end;

  TTestCaseIgnoresLeaksAtSetupLevel = class(TLeakTestClass)
  protected
    procedure SetUp; override;
  published
    procedure RunATest;
  end;

  TTestCaseIgnoresLeaksAtTearDownLevel = class(TLeakTestClass)
  protected
    procedure TearDown; override;
  published
    procedure RunATest;
  end;

  {-------------}
  TheTestClass = class of TLeakTestClass;

  TPassOrFail = class(TTestCase)
  private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    function CheckExecutionResult(const ATestClass: TheTestClass;
                                  const TestName: string;
                                  const CanDetectLeak: boolean;
                                  const Outcome: TExecutionStatus): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

{-------------}
  TTestMemoryMonitor = class(TPassOrFail)
  published
    procedure TestAlloweLeakListIterator;
    procedure TestNoLeaks;
    procedure TestSetupLeaks;
    procedure TestRunLeaks;
    procedure TestTearDownLeaks;
    procedure TestDefaultIsIgnoreLeaks;
    procedure TestContainsAllowedTObjectLeak;
    procedure TestContainsAllowedTObjectLeakByList;
    procedure TestContainsAllowedTObjectLeakByEmptyList;
    procedure TestContainsAllowedLeakArrayLongList;
    procedure TestCaseIgnoresLeaksAtSetupTearDownLevel;
  end;

  TLeaksAndPasses = class(TTestCase)
  private
    FSetUpObject   : TObject;
    FRunObject     : TObject;
    FTearDownObject: TObject;
    FRunObjectA    : TObject;
    FRunObjectB    : TObject;
    FSequenceNo: Integer;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ThisPassesBeforeSetUpLeak;
    procedure ThisFailsDueSetUpLeak;
    procedure ThisLeaksOnRun;
    procedure ThisPassesAfterRun;
    procedure ThisFailsDueTearDownLeak;
    procedure ThisPassesAfterPriorTearDownLeak;
    procedure ThisLeaksOnRunA;
    procedure ThisLeaksOnRunB;
    procedure ThisPassesAfterSucessiveLeaks;
  end;

  TContainsSuccessiveLeaks = class(TTestCase)
  private // Predecated on allowed leaks array size holding 3
    F1ObjsSize: Integer;
    F2ObjsSize: Integer;
    F3ObjsSize: Integer;

  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure Leak1Obj;
    procedure Leak2Obj;
    procedure Leak3Obj;
  end;

  TTestSuccessiveLeaksReport = class(TTestCase)
  private
    FSuccessiveLeaks: TContainsSuccessiveLeaks;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSuccessiveLeaksGetReported;
  end;

  // Created after observing tests in tiOPF2 within a TTestCase class
  // following a detected memory leak failed to run.
  TTestAllProcsRunAfterLeakyProcRuns = class(TTestCase)
  private
    FLeaksAndPasses: TLeaksAndPasses;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllProcsInTestCaseRunAfterLeakyProc;
  end;

  TAllowedAndDisallowedLeakyTests = class(TTestCase)
  private
    FObj1: TObject;
    FObj2: TObject;
    FObj3: TObject;
    FObj4: TObject;
  protected
    procedure SetUpOnce; override;
    procedure TearDownOnce; override;
  published
    procedure Proc1AllowedLeakPasses;
    procedure Proc2NotAllowedLeakFails;
    procedure Proc3ArrayAllowdLeakPasses;
    procedure Proc4NoArrayAllowedLeakFails;
  end;

  TTestAllowedLeaksGetZerored = class(TTestCase)
  private
    FAllowedAndDisallowedLeakyTests: TAllowedAndDisallowedLeakyTests;
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckAllowedLeakSizesGetZeroedBetweenTests;
  end;
 {$ENDIF}

  TestIProjectManager = class(TTestCase)
  private
    FIProjectManager: IProjectManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectManagerCreates;
  end;

  TestRegisterTestToProjectDefault = class(TTestCase)
  private
    FTestProject: ITestProject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectManagerOnNilITest;
  end;

  ITestProjectsRegistersDefaultAndNamedProject = interface(ITestCase)
  ['{F968AF3D-347D-46A8-AA28-F63DBDE675D2}']
    procedure TestProjectRegistersDefaultProjectMethod;
    procedure TestProjectsRegistersDefaultAndNamedProjectMethod;
  end;

  TestRegisterTestToNamedProject = class(TTestCase, ITestProjectsRegistersDefaultAndNamedProject)
  private
    FTestProject: ITestProject;
  protected
    procedure TestProjectRegistersDefaultProjectMethod;
    procedure TestProjectsRegistersDefaultAndNamedProjectMethod;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectRegistersDefaultProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectRegistersNamedProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectRegistersSplitNamedProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestProjectsRegistersDefaultAndNamedProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestDifferentNamedProjectsRegisterSameNamedTestSuitesSeparately;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestSingleNamedProjectRegistersSameNamedTestSuitesTogether;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestNamedProjectRegistersDifferentNamedTestSuitesSeparately;
  end;

  TTestEnableState = class(TTestCase)
  private
    FTestProject: ITestProject;
    FPath: string;
    FPathName: string;
    FProjectID: Integer;
    FProjectsManager: IProjectManager;
    FIniFile: Text;
    FLine: string;
    FFileOpened: boolean;

  function GetIniFileNextLine(const AFileName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestSaveEnableState = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithNoProjectsRegistered;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithOneTestRegisteredEnabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithOneTestRegisteredDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestsRegisteredEnabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestsRegistered1stDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestsRegistered2ndDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithOneTestSuiteRegisteredEnabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithOneTestSuiteRegisteredDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestSuitesRegistered2ndDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestSuitesRegistered2ndTestCaseDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithTwoTestSuitesRegistered2ndTestProcDisabled;
  end;

  TestSaveProjectEnableState = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithOneRegisteredProjectDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWith2ndRegisteredProjectDisabled;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CallSaveConfigurationWithAllNamedProjectTestsDisabled;
  end;

  TestLoadConfigurationDisablesTests = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadSingleTestMethod;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSecondTestMethod;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSingleTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSecondTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSingleTestSuite;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSecondTestSuite;
  end;

  TestLoadConfigurationExcludesTests = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSingleTestMethod;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSecondTestMethod;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSingleTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSecondTestCase;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSingleTestSuite;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludesSecondTestSuite;
  end;

  TestLoadConfigurationDisablesProjects = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSingleTestProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadDisablesSecondTestProject;
  end;

  TestLoadConfigurationExcludesProjects = class(TTestEnableState)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludedSingleTestProject;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure TestLoadExcludedSecondTestProject;
  end;

  TestMultiProject = class(TTestCase)
  private
    FITestedProjects: ITestProjectsRegistersDefaultAndNamedProject;
    FTestProject: ITestProject;
    FProjectsManager: IProjectManager;
    FMultiProjectSuite: ITestProject;
    FAnExecControl: ITestExecControl;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckNoMultiProjectOnDefaultOnly;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectGetsGivenExeName;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectCountsAndIteratesTests;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectCountAndIterateTestCases;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectExecControl;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectCountSetsDepths;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure CheckMultiProjectRunsSingleThreaded;
  end;

 {==============================================================================}
implementation
uses
  TypInfo,
  {$IFDEF SELFTEST}
  TestFramework,
  {$ENDIF}
  ProjectsManager,
  SharedTestClasses,
  TimeManager;

type
  EUnitTestsUnknownExcept = class(TestFramework.EDUnitException);
  EUnitTestsExpectedExcept = class(TestFramework.EDUnitException);

procedure RemoveProjectManager;
begin
  TestFramework.UnRegisterProjectManager;
end;


{ TTestITestProc }

procedure TTestITestProc.SetUp;
begin
  FTestProc := TestFramework.TTestProc.Create;
end;

procedure TTestITestProc.TearDown;
begin
  FTestProc := nil;
end;

procedure TTestITestProc.TestSet_Enabled;
begin
  FTestProc.Enabled := True;
  Check(FTestProc.Enabled, 'Failed to set enabled');
end;

procedure TTestITestProc.TestGet_Enabled;
begin
  FTestProc.Enabled := True;
  FTestProc.Enabled := False;
  Check(not FTestProc.Enabled, 'Failed to clear Enabled');
end;

procedure TTestITestProc.TestGet_Excluded;
begin
  FTestProc.Excluded := True;
  Check(FTestProc.Excluded, 'Failed to set Excluded');
end;

procedure TTestITestProc.TestSet_Excluded;
begin
  FTestProc.Excluded := True;
  FTestProc.Excluded := False;
  Check(not FTestProc.Excluded, 'Failed to clear Excluded');
end;

procedure TTestITestProc.TestSupportedType;
begin
  Check(FTestProc.SupportedIfaceType = _isTestMethod,
    'Supported interface should be _isTestMethod but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestProc.SupportedIfaceType)));
end;


{ TTestITestProcName }

procedure TTestITestProcName.SetUp;
begin
  inherited;
  FTestProc := TestFramework.TTestProc.Create;
end;

procedure TTestITestProcName.TearDown;
begin
  inherited;
  FTestProc := nil;
end;

procedure TTestITestProcName.TestDisplayedNameEmpty;
begin
  Check(FTestProc.DisplayedName <> '', 'Displayed Name empty on create');
end;

procedure TTestITestProcName.TestGetNameReturnsDisplayedName;
begin
  TestDisplayedNameEmpty;
  FTestProc.DisplayedName := 'ABCDEF';
  CheckEqualsString('ABCDEF', FTestProc.GetName,
    'GetName function should equal ABCDEF but retuned' + FTestProc.GetName);
end;

procedure TTestITestProcName.TestParentPathEmpty;
begin
  Check(FTestProc.DisplayedName <> '', 'Displayed Name empty on create');
end;

procedure TTestITestProcName.TestDisplayedName;
begin
  TestDisplayedNameEmpty;
  FTestProc.DisplayedName := 'TTestITestProcName.DisplayedName';
  CheckEqualsString('TTestITestProcName.DisplayedName', FTestProc.DisplayedName,
    'Displayed name does not equal FITestProc.DisplayedName');
end;

procedure TTestITestProcName.TestParentPath;
begin
  TestParentPathEmpty;
  FTestProc.ParentPath := 'TTestITestProcName.ParentPath';
  CheckEqualsString('TTestITestProcName.ParentPath', FTestProc.ParentPath,
    'Displayed name does not equal FITestProc.DisplayedName');
end;

{ TTestITestProcCreatesNamed }

procedure TTestITestProcCreatesNamed.SetUp;
begin
  FTestProc := nil;
end;

procedure TTestITestProcCreatesNamed.TearDown;
begin
  FTestProc := nil;
end;

procedure TTestITestProcCreatesNamed.TestTestProcCreatesEmptyNamed;
var
  LDummyTestMethod: TTestMethod;
begin
  LDummyTestMethod := nil;
  FTestProc := TestFramework.TTestProc.Create(LDummyTestMethod, '', TTestMethod(LDummyTestMethod), '');
  Check(FTestProc <> nil, 'Still nil after create attempt');
  Check(FTestProc.DisplayedName = '', 'Name should be empty for proc');
  Check(FTestProc.ParentPath = '', 'ParentPath should be empty');
  Check(FTestProc.Enabled = True, 'Should be enabled on create');
  Check(not FTestProc.IsTestMethod, 'Should not show as executable Method');
end;

procedure TTestITestProcCreatesNamed.TestTestProcCreatesNamed;
var
  LParentStr : string;
begin
  LParentStr := 'ParentPathStr';
  try
    FTestProc := TestFramework.TTestProc.Create(TestTestProcCreatesNamed, LParentStr,
      TestTestProcCreatesNamed, 'TestTestProcCreatesNamed');
    Check(FTestProc <> nil, 'Empty After create');
  except
    Check(False, 'Excepetion creating FITestProc');
  end;
  CheckEqualsString('TestTestProcCreatesNamed', FTestProc.DisplayedName,
    'Name should be <TestTestProcCreatesNamed> but was <' +
    FTestProc.DisplayedName + '>');
  CheckEqualsString(LParentStr, FTestProc.ParentPath,
    'Name should be <' + LParentStr + '> but was <' +
    FTestProc.ParentPath + '>');
  Check(FTestProc.Enabled = True, 'Should be enabled');
end;

{ TTestITestCaseExists }

procedure TTestITestCaseExists.TearDown;
begin
  inherited;
  FTestCase := nil;
end;

procedure TTestITestCaseExists.TestCreates;
begin
  try
    FTestCase := TestFramework.TTestCase.Create;
    Check(Assigned(FTestCase), 'Not created or assigned');
  except
    Check(False, 'Excepetion creating TTestProject');
  end;
  FTestCase := nil;
end;

procedure TTestITestCaseExists.TestDestroys;
begin
  FTestCase := TestFramework.TTestCase.Create;
  try
    FTestCase := nil;
    Check(FTestCase = nil, 'Failed to clear holding variable on destroy')
  except
    Check(False, 'Exception in TTestCase destructor');
  end;
end;

{ TTestDecoy }

procedure TTestDecoy.DecoyTest;
begin
  Check(False, 'This procedure should not be called');
end;

procedure TTestDecoy.ATestProcedure;
begin
  Check(True, 'If this fails we are in deep trouble');
end;

{ TTestNamedTestCase }

procedure TTestNamedTestCase.SetUp;
begin
  FTestCase := nil;
end;

procedure TTestNamedTestCase.TearDown;
begin
  FTestCase := nil;
end;

procedure TTestNamedTestCase.TestCreatesWithNoString;
begin
  FTestCase := TestFramework.TTestCase.Create;
  Check(Assigned(FTestCase), 'Constructor failed');
  Check(FTestCase.DisplayedName = 'TTestCase',
    'Name should be TTestCase but was ' + FTestCase.DisplayedName);
  Check(not Supports(FTestCase, ITestSuite), 'Must not be an ITestSuite');
  Check(FTestCase.SupportedIfaceType = _isTestCase,
    'Supported interface should be _isTestCase but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestCase.SupportedIfaceType)));
end;

procedure TTestNamedTestCase.TestCreatesWithBlankString;
begin
  FTestCase := TestFramework.TTestCase.Create('');
  Check(Assigned(FTestCase), 'Constructor failed');
  Check(FTestCase.DisplayedName = 'TTestCase',
    'Name should be TTestCase but was ' + FTestCase.DisplayedName);
  Check(not Supports(FTestCase, ITestSuite), 'Must not be an ITestSuite');
  Check(FTestCase.SupportedIfaceType = _isTestCase,
    'Supported interface should be _isTestCase but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestCase.SupportedIfaceType)));
end;

procedure TTestNamedTestCase.TestCreatesWithString;
begin
  FTestCase := TestFramework.TTestCase.Create('XYZ');
  Check(Assigned(FTestCase), 'Constructor failed');
  Check(FTestCase.DisplayedName = 'TTestCase',
    'Name should be TTestCase but was ' + FTestCase.DisplayedName);
  Check(not Supports(FTestCase, ITestSuite), 'Must not be an ITestSuite');
  Check(FTestCase.SupportedIfaceType = _isTestCase,
    'Supported interface should be _isTestCase but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestCase.SupportedIfaceType)));
end;

{ TTestNonPublished }

procedure TTestNonPublished.SetUp;
begin
  FTestCase := TTestDecoy.Create;
  FTestProject := nil;
end;

procedure TTestNonPublished.TearDown;
begin
  FreeAndNil(FTestCase);
  FTestProject := nil;
end;

procedure TTestNonPublished.TestNonPublishedProcedure;
var
  LTest: ITest;
begin
  Check(Assigned(FTestCase), 'FTestCase not created');
  LTest := FTestCase.FindNextEnabledProc;
  Check(Assigned(LTest), 'LTest = nil, should be assigned');
  Check(LTest.DisplayedName <> 'DecoyTest',
    'Tests may have {$M+} set and name should be <ATestProcedure> but is <' +
      LTest.DisplayedName + '>');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Test procedure name should be <ATestProcedure> but is <' +
      LTest.DisplayedName + '> because non published procedure has been included as published');
end;

{ TTestCase1 }

procedure TTestCase1.ATestProcedure;
begin
  FLocalLevel := Depth;
end;

function TTestCase1.get_LocalLevel: Integer;
begin
  Result := FLocalLevel;
end;

{ TTestParentPathPopulatesTests }

destructor TTestParentPathPopulatesTests.Destroy;
begin
  FTestCase1 := nil;
  inherited;
end;

procedure TTestParentPathPopulatesTests.SetUp;
begin
  FTestCase1 := TTestCase1.Create;
end;

procedure TTestParentPathPopulatesTests.TearDown;
begin
  FTestCase1 := nil;
end;

procedure TTestParentPathPopulatesTests.TestParentPathSet;
var
  LTest: ITest;
begin
  Check(Assigned(FTestCase1), 'FTestCase1 not created');
  Check(FTestCase1.ParentPath = '',
    'ParentPath should be empty but was <' + FTestCase1.ParentPath + '>');

  LTest := FTestCase1.FindNextEnabledProc;
  Check(Assigned(LTest), 'LTest = nil, should be assigned');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Test procedure name should be <ATestProcedure> but is <' +
      LTest.DisplayedName + '>');
  Check(LTest.ParentPath = FTestCase1.DisplayedName,
    'Test procedure name should be <' + FTestCase1.DisplayedName + '> but is <' +
      LTest.ParentPath + '>');
end;

{ TTestLevelTestRunsFrom }

destructor TTestLevelTestRunsFrom.Destroy;
begin
  FTestCase1 := nil;
  inherited;
end;

procedure TTestLevelTestRunsFrom.SetUp;
begin
  FAnExecControl := nil;
  FTestCase1 := TTestCase1.Create;
  FTestProject := nil;
end;

procedure TTestLevelTestRunsFrom.TearDown;
begin
  FTestCase1 := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestLevelTestRunsFrom.TestATestProcLevel;
var
  LCount: Integer;
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('SuiteDepthCheck', [TTestCase1.Suite, TTestCase3.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(FTestProject.Depth = 0,
    'Depth should be 0 but is ' + IntToStr(FTestProject.Depth));

  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'SuiteDepthCheck',
    'Name should be SuiteDepthCheck but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 1,
    'Depth should be 1 but is ' + IntToStr(LTest.Depth));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase1',
    'Name should be TTestCase1 but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 2,
    'Depth should be 2 but is ' + IntToStr(LTest.Depth));

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  Check((LTest as ITestCase1).get_LocalLevel = 2,
    'Should be 2 but is ' + IntToStr(FTestCase1.get_LocalLevel));

  (LTest as ITestCase).Run(FAnExecControl);
  // Double check we run test methods from the owning testcase depth
  Check((LTest as ITestCase1).get_LocalLevel = 2,
    'Should be 2 but is ' + IntToStr(FTestCase1.get_LocalLevel));
end;

procedure TTestLevelTestRunsFrom.TestMethodDepths;
var
  LCount: Integer;
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('SuiteDepthCheck', [TTestCase1.Suite,
                                                   TTestCase3.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(FTestProject.Depth = 0,
    'Depth should be 0 but is ' + IntToStr(FTestProject.Depth));

  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'SuiteDepthCheck',
    'Name should be SuiteDepthCheck but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 1, 'Depth should be 1 but is ' + IntToStr(LTest.Depth));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase1',
    'Name should be TTestCase1 but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 2, 'Depth should be 2 but is ' + IntToStr(LTest.Depth));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Name should be ATestProcedure but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 3, 'Depth should be 3 but is ' + IntToStr(LTest.Depth));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3',
    'Name should be TTestCase1 but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 2, 'Depth should be 2 but is ' + IntToStr(LTest.Depth));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Name should be ATestProcedure but is ' + LTest.DisplayedName);
  Check(LTest.Depth = 3, 'Depth should be 3 but is ' + IntToStr(LTest.Depth));
end;

{ TTestCase2 }

procedure TTestCase2.SetUp;
begin
  inherited;
  FSetUpRan := True;
end;

procedure TTestCase2.TearDown;
begin
  inherited;
  FTearDownRan := True;
end;

function TTestCase2.Get_SetupRan: boolean;
begin
  Result := FSetUpRan;
end;

function TTestCase2.Get_RunRan: boolean;
begin
  Result := FRunRan;
end;

function TTestCase2.Get_TearDownRan: boolean;
begin
  Result := FTearDownRan;
end;

function TTestCase2.Get_ATestProcRan: boolean;
begin
  Result := FTestProcRan;
end;

procedure TTestCase2.ATestProcedure;
begin
  FTestProcRan := True;
end;

function TTestCase2.Run(const ExecControl: ITestExecControl): TExecutionStatus;
begin
  Result := inherited Run(ExecControl);
  FRunRan := True;
end;

{ TTestATTestCase }

destructor TTestATTestCase.Destroy;
begin
  FTestCase2 := nil;
  inherited;
end;

procedure TTestATTestCase.SetUp;
begin
  FAnExecControl := nil;
  FTestCase2 := TTestCase2.Create;
end;

procedure TTestATTestCase.TearDown;
begin
  inherited;
  FAnExecControl := nil;
  FTestCase2 := nil;
end;

procedure TTestATTestCase.TestSetUpRan;
begin
  FTestCase2.SetUp;
  Check(FTestCase2.Get_SetUpRan, 'Setup failed to run');
end;

procedure TTestATTestCase.TestATestProcRan;
begin
  TestRunRan;
  Check(FTestCase2.Get_ATestProcRan, 'ATestProc failed to run');
end;

procedure TTestATTestCase.TestCreateDestroyLeaks;
begin
  try
    FTestCase2 := nil;
    Check(FTestCase2 = nil, 'Failed to free');
  except
    Check(False, 'Failed during destruction');
  end;
end;

procedure TTestATTestCase.TestRunRan;
begin
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase2.Run(FAnExecControl);
  Check(FTestCase2.Get_RunRan, 'Run failed to run');
end;

procedure TTestATTestCase.TestTearDownRan;
begin
  FTestCase2.TearDown;
  Check(FTestCase2.Get_TearDownRan, 'TearDown failed to run');
end;


{ TTestCase3 }

function TTestCase3.Get_TestProcRan: Integer;
begin
  Result := FTestProcRan;
end;

procedure TTestCase3.TestProcedure1;
begin
  Inc(FTestProcRan ,1);
end;

procedure TTestCase3.TestProcedure2;
begin
  Inc(FTestProcRan ,2);
end;


{ TTest2TTestCase }

destructor TTest2TTestCase.Destroy;
begin
  FTestCase3 := nil;
  inherited;
end;

procedure TTest2TTestCase.SetUp;
begin
  FAnExecControl := nil;
  FTestCase3 := TTestCase3.Create;
end;

function TTest2TTestCase.ShouldRunTest(const ATest: ITest): boolean;
begin
  Result := FCanRun;
end;

procedure TTest2TTestCase.TearDown;
begin
  FTestCase3 := nil;
  FAnExecControl := nil;
  RemoveProjectManager;
end;

procedure TTest2TTestCase.TestTestProcsRan;
var
  Lretval: Integer;
begin
  Check(FTestCase3.Count = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestCase3.Count ));
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase3.Run(FAnExecControl);
  Lretval := FTestCase3.Get_TestProcRan;
  Check((Lretval = 3),
    'One or more procedures did not execute. Return value = ' + IntToStr(Lretval));
end;

procedure TTest2TTestCase.TestGetsNextEnabledTest;
var
  LTest: ITest;
begin
  Check(Assigned(FTestCase3), 'FTestCase3 should not be nil');
  LTest := FTestCase3.FindNextEnabledProc;
  Check(Assigned(LTest), 'NextEnabledProc should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'DisplayName should be TestProcedure1 but is ' + LTest.DisplayedName);

  LTest := FTestCase3.FindNextEnabledProc;
  Check(Assigned(LTest), 'NextEnabledProc should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'DisplayName should be TestProcedure2 but is ' + LTest.DisplayedName);

  LTest := FTestCase3.FindNextEnabledProc;
  Check(LTest = nil, 'NextEnabledProc should be nil');
end;

procedure TTest2TTestCase.TestOnlyRunsEnabledTest;
var
  Lretval: Integer;
begin
  Check(FTestCase3.Count = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestCase3.Count ));
  FTestCase3.Reset;
  FTestCase3.FindNextEnabledProc.Enabled := False;
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase3.Run(FAnExecControl);
  Lretval := FTestCase3.Get_TestProcRan;
  Check((Lretval = 2),
    'Incorrect number of procedures executes. Return value = ' + IntToStr(Lretval));
end;

procedure TTest2TTestCase.TestOnlyRunsSingleEnabledTest;
var
  Lretval: Integer;
begin
  Check(FTestCase3.Count = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestCase3.Count ));
  FTestCase3.Reset;
  FCanRun := False;
  Check(not ShouldRunTest(nil),
    'function ShouldRunTest return value should be False');

  FAnExecControl := TestFramework.TestExecControl;
  Check(Assigned(FAnExecControl), ' FAnExecControl not assigned');
  FAnExecControl.IndividuallyEnabledTest := ShouldRunTest;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase3.Run(FAnExecControl);
  Lretval := FTestCase3.Get_TestProcRan;
  Check((Lretval = 0),
    'Neither procedure should have executed. Return value = ' + IntToStr(Lretval));

  FTestCase3.Reset;
  FCanRun := True;
  Check(ShouldRunTest(nil),
    'function ShouldRunTest return value should be True');
  FAnExecControl.IndividuallyEnabledTest := ShouldRunTest;
  FTestCase3.Run(FAnExecControl);
  Lretval := FTestCase3.Get_TestProcRan;
  Check((Lretval = 3),
    'Both procedures should have executed. Return value = ' + IntToStr(Lretval));
end;


{ TTest3TTestCase }

destructor TTest3TTestCase.Destroy;
begin
  FTestCase3 := nil;
  inherited;
end;

function TTest3TTestCase.Get_TestLevelRan: Integer;
begin
  Result := FTestProcsRan;
end;

procedure TTest3TTestCase.SetUp;
begin
  inherited;
  FAnExecControl := nil;
  FTestCase3 := TTestCase3.Create;
end;

procedure TTest3TTestCase.TearDown;
begin
  FAnExecControl := nil;
  FTestCase3 := nil;
  inherited;
end;

procedure TTest3TTestCase.TestTestProcsRan;
begin
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase3.Run(FAnExecControl);
  FTestProcsRan := FTestCase3.Get_TestProcRan;
end;


{ TTest3LevelsCase }

destructor TTest3LevelsCase.Destroy;
begin
  FTest3TTestCase := nil;
  inherited;
end;

procedure TTest3LevelsCase.SetUp;
begin
  FAnExecControl := nil;
  FTest3TTestCase := TTest3TTestCase.Create;
end;

procedure TTest3LevelsCase.TearDown;
begin
  FAnExecControl := nil;
  FTest3TTestCase := nil;
  RemoveProjectManager;
end;

procedure TTest3LevelsCase.TestTestProcsRan;
var
  Lretval: Integer;
begin
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTest3TTestCase.Run(FAnExecControl);
  Lretval := FTest3TTestCase.Get_TestLevelRan;
  Check((Lretval = 3),
    'Procedures did not execute. Total should be 3 but is ' + IntToStr(Lretval));
end;


{ TChecksGetCurrentTestCasePath }

procedure TChecksGetCurrentTestCasePath.ChecksGetName;
var
  LGetName: string;
begin
  LGetName := GetName;
  CheckEqualsString('Overriding works ChecksGetName', LGetName, '');
end;

function TChecksGetCurrentTestCasePath.GetName: string;
begin
  Result := 'Overriding works ' + inherited GetName;
end;


{ TVerifyOverriddenGetNameIsAccessed }

function TVerifyOverriddenGetNameIsAccessed.GetName: string;
begin
  Result := 'ABC ' + inherited GetName;
end;

procedure TVerifyOverriddenGetNameIsAccessed.SetUp;
begin
  FTestCase := nil;
  FAnExecControl := nil;
  FInCallbackGetsExecutedOverriddenProcName := '';
  FInCallbackGetsExecutedTestCaseName := '';
  FInCallbackGetsProcName := '';
  FInCallbackGetsThisOverriddenProcName := '';
  FInCallbackGetsThisTestCaseName := '';
  FInCallbackGetsThisInheritedTestCaseName := '';
end;

procedure TVerifyOverriddenGetNameIsAccessed.TearDown;
begin
  FInCallbackGetsThisInheritedTestCaseName := '';
  FInCallbackGetsThisTestCaseName := '';
  FInCallbackGetsThisOverriddenProcName := '';
  FInCallbackGetsProcName := '';
  FInCallbackGetsExecutedTestCaseName := '';
  FInCallbackGetsExecutedOverriddenProcName := '';
  FAnExecControl := nil;
  FTestCase := nil;
end;

procedure TVerifyOverriddenGetNameIsAccessed.ExecStatusCallBack(const ATest: ITest);
begin
  // The callback occurs within the contect of the TTestCase
  if (ATest.isTestMethod and (ATest.ExecStatus = _Passed)) then
  begin
    FInCallbackGetsExecutedTestCaseName := ATest.DisplayedName;
    FInCallbackGetsExecutedOverriddenProcName := ATest.GetName;
    FInCallbackGetsThisOverriddenProcName := GetName;
    FInCallbackGetsProcName := inherited GetName;
    FInCallbackGetsThisTestCaseName := DisplayedName;
    FInCallbackGetsThisInheritedTestCaseName := inherited DisplayedName;
    FCallbackProcDepth := Depth;
    FTestProcDepth := ATest.ParentTestCase.Depth;
  end;
end;

procedure TVerifyOverriddenGetNameIsAccessed.VerifyCallbackTestCaseName;
var
  LReturnStatus: TExecutionStatus;
begin
  FTestCase := TChecksGetCurrentTestCasePath.Create;
  FTestCase.Depth := Depth + 1;
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBack;
  LReturnStatus := FTestCase.Run(FAnExecControl);

  Check(LReturnStatus = _Passed, 'Returned status should be _Passed but was ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  CheckEqualsString('ChecksGetName', FInCallbackGetsExecutedOverriddenProcName, 'Error in callback ');
  CheckEqualsString(FInCallbackGetsThisTestCaseName, FInCallbackGetsThisInheritedTestCaseName,
    'DisplayedName and inherited DisplayedName should be equal');
  CheckEqualsString(FInCallbackGetsThisTestCaseName, 'TVerifyOverriddenGetNameIsAccessed',
    'DisplayedName and TTestCase name should be equal');
  CheckEqualsString(FInCallbackGetsThisOverriddenProcName, 'ABC VerifyCallbackTestCaseName',
    'DisplayedName and TTestCase name should be equal');
  Check(FCallbackProcDepth + 1 = FTestProcDepth, 'Depth missmatch');
end;

{ TExecutableTestCase }

function TExecutableTestCase.GetName: string;
begin
  Result := 'TESTCASE ' + inherited GetName;
end;

procedure TExecutableTestCase.Passes;
begin
  Check(True);
end;


{ TVerifyGetNameInSuiteIsAccessed }

function TVerifyGetNameInSuiteIsAccessed.GetName: string;
begin
  Result := 'LMN ' + inherited GetName;
end;

procedure TVerifyGetNameInSuiteIsAccessed.SetUp;
begin
  FTestSuite := nil;
  FTestProcDepth := -1;
  FInCallbackGetsExecutedOverriddenProcName := '';
  FInCallbackGetsExecutedTestCaseName := '';
end;

procedure TVerifyGetNameInSuiteIsAccessed.TearDown;
begin
  FInCallbackGetsExecutedTestCaseName := '';
  FInCallbackGetsExecutedOverriddenProcName := '';
  FTestSuite := nil;
end;

// This test verifies GetName is returned from the active test method and not
// the active tests case.

// Catch the callback within the contect of the TTestCase
procedure TVerifyGetNameInSuiteIsAccessed.ExecStatusCallBack1(const ATest: ITest);
begin
  if (ATest.IsTestMethod) and (ATest.ExecStatus = _Passed) then
  begin
    FInCallbackGetsExecutedTestCaseName := ATest.ParentTestCase.DisplayedName;
    FInCallbackGetsExecutedOverriddenProcName := ATest.ParentTestCase.GetName;
    FCallbackProcDepth := Depth;
    FTestProcDepth := ATest.ParentTestCase.Depth;
  end;
end;

procedure TVerifyGetNameInSuiteIsAccessed.VerifyCallbackTestSuiteName;
var
  LReturnStatus: TExecutionStatus;
  LITestCase: ITestCase;
  LExecControl: ITestExecControl;
begin
  FTestSuite := TestFramework.TTestSuite.Suite('TSName');
  LITestCase := TExecutableTestCase.Create;
  FTestSuite.Depth := Depth + 1;
  LITestCase.Depth := FTestSuite.Depth + 1;
  FTestSuite.AddTest(LITestCase);
  LExecControl := TestFramework.TestExecControl;
  LExecControl.ExecStatusUpdater := ExecStatusCallBack1;
  {$IFDEF FASTMM}
    LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestSuite.Run(LExecControl);

  Check(LReturnStatus = _Passed, 'Returned status should be _Passed but was ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  CheckEqualsString('TExecutableTestCase', FInCallbackGetsExecutedTestCaseName, 'Error in callback ');
  CheckEqualsString('TESTCASE Passes', FInCallbackGetsExecutedOverriddenProcName, 'Error in callback ');
  Check(FCallbackProcDepth < FTestProcDepth, 'Depth missmatch');
end;

// This test is not very clear but it verifies the correct content of the
// ParentCase property which allows GetName to use the inheritance mechanism to
// overload lower GetName properties to decorate names.

// Catch the callback within the contect of the TTest method
procedure TVerifyGetNameInSuiteIsAccessed.ExecStatusCallBack2(const ATest: ITest);
begin
  // The callback occurs within the contect of the TTestCase
  if (ATest.ParentTestCase <> nil) and (ATest.DisplayedName = 'Passes') then
  begin
    FInCallbackGetsExecutedTestCaseName := ATest.ParentTestCase.DisplayedName;
    FInCallbackGetsExecutedOverriddenProcName := ATest.ParentTestCase.GetName;
    FCallbackProcDepth := Depth;
    FTestProcDepth := ATest.ParentTestCase.Depth;
  end;
end;

// Note. Prepending text to GetName function causes a potential mem leak so
// _Warning is returned

procedure TVerifyGetNameInSuiteIsAccessed.VerifyCallbackTestSuiteOverriddenName;
var
  LReturnStatus: TExecutionStatus;
  LITestCase: ITestCase;
  LExecControl: ITestExecControl;
begin
  FTestSuite := TestFramework.TTestSuite.Suite('TSName');
  LITestCase := TExecutableTestCase.Create;
  FTestSuite.Depth := Depth + 1;
  LITestCase.Depth := FTestSuite.Depth + 1;
  FTestSuite.AddTest(LITestCase);
  LExecControl := TestFramework.TestExecControl;
  LExecControl.ExecStatusUpdater := ExecStatusCallBack2;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestSuite.Run(LExecControl);

  Check(LReturnStatus in [_Passed, _Warning],
    'Returned status should be _Passed or _Warning but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  CheckEqualsString('TExecutableTestCase', FInCallbackGetsExecutedTestCaseName, 'Error in callback ');
  CheckEqualsString('TESTCASE Passes', FInCallbackGetsExecutedOverriddenProcName, 'Error in callback ');
  Check(FCallbackProcDepth < FTestProcDepth, 'Depth missmatch');
end;


{ TTestTestCaseCanCountAndRetrieveList }

procedure TTestTestCaseCanCountAndRetrieveList.SetUp;
begin
  FTestCase := TTestCase3.Create;
end;

procedure TTestTestCaseCanCountAndRetrieveList.TearDown;
begin
  FTestCase := nil;
end;

procedure TTestTestCaseCanCountAndRetrieveList.VerifyCountReturnsTotalEnabledTestMethodCount;
var
  LCount: Integer;
begin
  Check(Assigned(FTestCase), 'TestCase not created');
  (FTestCase as ITestCase).Reset;
  Check((FTestCase as ITestCase).Count = 2, ' Count should be 2');
  LCount := (FTestCase as ITestCase).Count;
  Check(LCount = 2,
    'Count not 2 on second attempt, count = ' + IntToStr(LCount));
end;

procedure TTestTestCaseCanCountAndRetrieveList.VerifyPriorTestFetchesTestMethodsInReverseOrder;
var
  LTest: ITest;
  LName: string;
begin
  VerifyCountReturnsTotalEnabledTestMethodCount; // Leaves iterator index at max count
  LTest := (FTestCase as ITestCase).PriorTest;
  Check(LTest <> nil, 'Should not be nil');
  LName := LTest.DisplayedName;
  Check(LName = 'TestProcedure2',
    ' Name should be TestProcedure2 but is ' + LName);

  LTest := (FTestCase as ITestCase).PriorTest;
  Check(LTest <> nil, 'Should not be nil');
  LName := LTest.DisplayedName;
  Check(LName = 'TestProcedure1',
    ' Name should be TestProcedure1 but is ' + LName);
end;

procedure TTestTestCaseCanCountAndRetrieveList.VerifyResetResetsTestMethodIterator;
var
  LTest: ITest;
begin
  VerifyCountReturnsTotalEnabledTestMethodCount;
  (FTestCase as ITestCase).Reset;
  LTest := (FTestCase as ITestCase).PriorTest;
  Check(LTest = nil, 'Should be nil');
end;

{ TTestWithRunTestMethod }

function TTestWithRunTestMethod.HasRun: boolean;
begin
  Result := FHasRun;
end;

procedure TTestWithRunTestMethod.RunTest;
begin
  inherited;
  FHasRun := True;
  Check(True);
end;

{ TTestLegacyRunTestMethod }

procedure TTestLegacyRunTestMethod.SetUp;
begin
  FSuite := nil;
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestLegacyRunTestMethod.TearDown;
begin
  FSuite := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestLegacyRunTestMethod.TestLegacyAbstractTestRegisters;
var
  LCount: Integer;
begin
  FSuite := TTestSuiteRunsTestMethod.Suite;
  FSuite.AddTest(TTestWithRunTestMethod.Create('RunTest1'));
  FSuite.AddTest(TTestWithRunTestMethod.Create('RunTest2'));
  LCount := FSuite.CountTestCases;
  Check(LCount = 2,
    'Should be 2 test cases present but there were ' + IntToStr(LCount));
  TestFramework.RegisterTest(FSuite);
  FTestProject := TestFramework.Projects;
  Check(FTestProject <> nil, 'TestProject should exist');
  LCount := FTestProject.Count;
  Check(LCount = 2,
    'Should be 2 test cases present but there were ' + IntToStr(LCount));
  FSuite := nil;
end;

procedure TTestLegacyRunTestMethod.TestRunTestRan;
begin
  TestLegacyAbstractTestRegisters;
  Check(Assigned(FTestProject), 'TestProject should exist');
  FAnExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
    AllowedMemoryLeakSize := 24; // Don't know where it comes from, yet.
    FAnExecControl.InhibitStackTrace := TestFramework.TestExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(FAnExecControl);
  Check(FAnExecControl.ExecutionCount = 2,
    'Count of tests run should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));
end;

{ TTestWithRunTestFailedCheck }

function TTestWithRunTestFailedCheck.HasRun: boolean;
begin
  Result := FHasRun;
end;

procedure TTestWithRunTestFailedCheck.RunTest;
begin
  inherited;
  Check(False, 'Test must fail');
end;

{ TTestLegacyRunTestMethodCheckFails }

procedure TTestLegacyRunTestMethodCheckFails.SetUp;
begin
  inherited;
  FSuite := nil;
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestLegacyRunTestMethodCheckFails.TearDown;
begin
  inherited;
  FSuite := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestLegacyRunTestMethodCheckFails.TestRunTestFailedCheck;
var
  LStatus: TExecutionStatus;
  LCount: Integer;
begin
  FSuite := TTestSuiteRunsTestMethod.Suite;
  FSuite.AddTest(TTestWithRunTestFailedCheck.Create('RunTest1'));
  FSuite.AddTest(TTestWithRunTestFailedCheck.Create('RunTest2'));
  LCount := FSuite.CountTestCases;
  Check(LCount = 2,
    'Should be 2 test cases present but there were ' + IntToStr(LCount));
  TestFramework.RegisterTest(FSuite);
  FTestProject := TestFramework.Projects;
  Check(FTestProject <> nil, 'TestProject should exist');
  LCount := FTestProject.Count;
  Check(LCount = 2,
    'Should be 2 test cases present but there were ' + IntToStr(LCount));
  FSuite := nil;
  Check(Assigned(FTestProject), 'TestProject should exist');

  FAnExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
    FAnExecControl.InhibitStackTrace := TestFramework.TestExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Failed,
    'TestStatus should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  Check(FAnExecControl.ExecutionCount = 2,
    'Count of tests run should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.FailureCount = 2,
    'Count of Failures should be 2 but was ' + IntToStr(FAnExecControl.WarningCount));
end;

procedure TTestLegacyRunTestMethodCheckFails.TestCanDisableRunTest;
var
  LStatus: TExecutionStatus;
  LCount: Integer;
  LTest: ITest;
begin
  FSuite := TTestSuiteRunsTestMethod.Suite;
  FSuite.AddTest(TTestWithRunTestFailedCheck.Create('RunTest1'));
  LTest := TTestWithRunTestFailedCheck.Create('RunTest2');
  FSuite.AddTest(LTest);
  FSuite.AddTest(TTestWithRunTestFailedCheck.Create('RunTest3'));
  LCount := FSuite.CountTestCases;
  Check(LCount = 3,
    'Should be 3 test cases present but there were ' + IntToStr(LCount));
  TestFramework.RegisterTest(FSuite);
  FTestProject := TestFramework.Projects;
  Check(FTestProject <> nil, 'TestProject should exist');
  LCount := FTestProject.Count;
  Check(LCount = 3,
    'Should be 3 test cases present but there were ' + IntToStr(LCount));

  LTest.Enabled := False;

  FAnExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
    AllowedMemoryLeakSize := 24; // Don't know where it comes from, yet.
    FAnExecControl.InhibitStackTrace := TestFramework.TestExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Failed,
    'TestStatus should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  Check(FAnExecControl.ExecutionCount = 2,
    'Count of tests run should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.FailureCount = 2,
    'Count of Failures should be 2 but was ' + IntToStr(FAnExecControl.WarningCount));
end;

{ TTestITestSuiteExists }

procedure TTestITestSuiteExists.TestCreates;
begin
  try
    FTestSuite := TestFramework.TTestSuite.Create;
    Check(Assigned(FTestSuite), 'Not created or assigned');
  except
    FTestSuite := nil;
    Check(False, 'Excepetion creating FITestSuite');
  end;
  if Assigned(FTestSuite) then
    Check(FTestSuite.DisplayedName = 'TTestSuite', 'UnNamed TestSuite create should return TTestSuite');
  Check(FTestSuite.SupportedIfaceType = _isTestSuite,
    'Supported interface should be _isTestSuite but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestSuite.SupportedIfaceType)));
  FTestSuite := nil;
end;

procedure TTestITestSuiteExists.TestCreatesEmptyName;
begin
  try
    FTestSuite := TestFramework.TTestSuite.Suite('');
    Check(Assigned(FTestSuite), 'Not created or assigned');
  except
    Check(False, 'Excepetion creating FITestSuite');
  end;
  if Assigned(FTestSuite) then
    Check(FTestSuite.DisplayedName = 'TTestSuite', 'Empty string named TestSuite create should return TTestSuite');
  FTestSuite := nil;
end;

procedure TTestITestSuiteExists.TestCreatesHasName;
begin
  try
    FTestSuite := TestFramework.TTestSuite.Suite('NamedSuite');
    Check(Assigned(FTestSuite), 'Not created or assigned');
  except
    Check(False, 'Excepetion creating FITestSuite');
  end;
  if Assigned(FTestSuite) then
    Check(FTestSuite.DisplayedName = 'NamedSuite', 'Named TestSuite create should return NamedSuite');
  FTestSuite := nil;
end;

procedure TTestITestSuiteExists.TestDestroys;
begin
  FTestSuite := TestFramework.TTestSuite.Create;
  try
    FTestSuite := nil;
    Check(FTestSuite = nil, 'Failed to clear holding variable on destroy')
  except
    Check(False, 'Exception in FITestSuite destructor');
  end;
end;

procedure TTestITestSuiteExists.TearDown;
begin
  inherited;
  FTestSuite := nil;
end;

{ TTestTestSuiteAddMethods }

procedure TTestITestSuiteAddMethods.TearDown;
begin
  FSuite := nil;
  FTestSuite := nil;
  RemoveProjectManager;
end;

procedure TTestITestSuiteAddMethods.TestCheckNameOnAddSuite;
var
  LTest : ITestCase;
begin
  LTest := nil;
  FTestSuite := TestFramework.TTestSuite.Create;
  Check(Assigned(FTestSuite), 'ATestSuite not created');
  FSuite := TTestCase3.Create;
  Check(Assigned(FSuite), 'FSuite was not created');
  FTestSuite.DisplayedName := 'TTestSuite';
  FTestSuite.AddTest(LTest);
  Check(FTestSuite.DisplayedName = 'TTestSuite',
    'Name should not change for nil object add');
  FTestSuite.DisplayedName := 'TTestSuite';
  FTestSuite.AddTest(FSuite);
  Check(FTestSuite.DisplayedName <> '',
    'Name should not be nil for object add with no string');
  FTestSuite.DisplayedName := 'TTestSuite';
  FTestSuite.AddTest('', FSuite);
  Check(FTestSuite.DisplayedName <> '',
    'Name should not be nil for object add with empty string');
end;

procedure TTestITestSuiteAddMethods.TestGetsNextEnabledTest;
var
  LTest : ITest;
begin
  FTestSuite := TestFramework.TTestSuite.Create;
  FSuite := TTestCase3.Create;
  FTestSuite.AddTest('', FSuite);
  Check(FTestSuite.DisplayedName <> '',
    'Name should not be empty for object add with empty string');
  Check(FTestSuite.DisplayedName = 'TTestSuite',
    'Name should be TTestSuite but is ' + FTestSuite.DisplayedName);
  LTest := FTestSuite.FindNextEnabledProc;
  Check(Assigned(LTest), 'NextEnabledProc should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Name should be TestProcedure1 but is ' + LTest.DisplayedName);

  LTest := FTestSuite.FindNextEnabledProc;
  Check(Assigned(LTest), 'NextEnabledProc should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Name should be TestProcedure2 but is ' + LTest.DisplayedName);

  LTest := FTestSuite.FindNextEnabledProc;
  Check(LTest = nil, 'NextEnabledProc should be nil');
end;

{ TTestSuiteCanCountAndList }

procedure TTestITestSuiteCanCountAndList.SetUp;
begin
  inherited;
  FTestSuite := TestFramework.TTestSuite.Create;
  FTestCase := TTestCase3.Create;
  FTestSuite.AddTest(FTestCase);
end;

procedure TTestITestSuiteCanCountAndList.TearDown;
begin
  FTestCase := nil;
  FTestSuite := nil;
  RemoveProjectManager;
end;

procedure TTestITestSuiteCanCountAndList.VerifyTestSuiteCanCountTestMethods;
var
  LCount: Integer;
begin
  Check(Assigned(FTestSuite), 'TestSuite not created');
  Check(Assigned(FTestCase), 'TestCase not created');
  Check(FTestCase.Count = 2, ' Count should be 2');
  LCount := FTestSuite.Count;
  Check(LCount = 2,
    'Count executed on newly created TestSuite should be 2 but was ' + IntToStr(LCount));
end;

procedure TTestITestSuiteCanCountAndList.VerifyPriorTestFetchesTestMethodsInReverseOrder;
var
  LTest: ITest;
  LName: string;
begin
  VerifyTestSuiteCanCountTestMethods;
  LTest := FTestSuite.PriorTest;
  Check(LTest <> nil, 'Should not be nil');
  LName := LTest.DisplayedName;
  Check(LName = 'TestProcedure2',
    ' Name should be TestProcedure2 but is ' + LName);

  LTest := FTestSuite.PriorTest;
  Check(LTest <> nil, 'Should not be nil');
  LName := LTest.DisplayedName;
  Check(LName = 'TestProcedure1',
    ' Name should be TestProcedure1 but is ' + LName);

  LTest := FTestSuite.PriorTest;
  Check(LTest <> nil, 'Should not be nil');
  LName := LTest.DisplayedName;
  Check(LName = 'TTestCase3',
    ' Name should be TTestCase3 but is ' + LName);

  LTest := FTestSuite.PriorTest;
  Check(LTest = nil, 'Reset failed. Returned result be nil');
end;

procedure TTestITestSuiteCanCountAndList.VerifyTestSuiteCanBeReset;
var
  LTest: ITest;
begin
  Check(Assigned(FTestSuite), 'TestSuite not created');
  FTestSuite.Count; // This currently leaves ther iterator pointers at max count
  (FTestCase as ITestCase).Reset;
  LTest := (FTestCase as ITestCase).PriorTest;
  Check(LTest = nil, 'TestSuite was not reset as required');
end;

procedure TTestITestSuiteCanCountAndList.VerifyTestSuiteDoesNotCountDisabledTestMethods;
var
  LTest: ITest;
  LCount: Integer;
begin
  LCount := FTestSuite.Count; // Note. This is the count of enabled tests
  Check(LCount = 2,
    'Count executed on newly created TestSuite should be 2 but was ' + IntToStr(LCount));
  LCount := 0;
  FTestSuite.Reset;
  LTest := FTestCase.FindNextEnabledProc;
  while Assigned(LTest) do
  begin
    LTest.Enabled := False;
    Inc(LCount);
    LTest := FTestSuite.FindNextEnabledProc;
  end;
  Check(LCount = 2, 'Setup condition requires 2 disabled test methods');

  LCount := 0;
  FTestSuite.Reset;
  LTest := FTestSuite.FindNextEnabledProc;
  while Assigned(LTest) do
  begin
    Inc(LCount);
    LTest := FTestSuite.FindNextEnabledProc;
  end;
  Check(LCount = 0, 'FindNextEnabledProc should not have found any enabled tests but found ' + IntToStr(LCount));

  LCount := FTestSuite.Count;
  Check(LCount = 0,
    'Count executed with no enabled test methods should be 0 but was ' + IntToStr(LCount));
end;

procedure TTestITestSuiteCanCountAndList.VerifyTestSuiteDoesNotCountExcludedTestMethods;
var
  LTest: ITest;
  LCount: Integer;
begin
  LCount := FTestSuite.Count; // Note. This is the count of enabled tests
  Check(LCount = 2,
    'Count executed on newly created TestSuite should be 2 but was ' + IntToStr(LCount));
  LCount := 0;
  FTestSuite.Reset;
  LTest := FTestCase.FindNextEnabledProc;
  while Assigned(LTest) do
  begin
    LTest.Excluded := True;
    Inc(LCount);
    LTest := FTestSuite.FindNextEnabledProc;
  end;
  Check(LCount = 2, 'Setup condition requires 2 disabled test methods');

  LCount := 0;
  FTestSuite.Reset;
  LTest := FTestSuite.FindNextEnabledProc;
  while Assigned(LTest) do
  begin
    Inc(LCount);
    LTest := FTestSuite.FindNextEnabledProc;
  end;
  Check(LCount = 2, 'FindNextEnabledProc should have found 2 enabled tests but found ' + IntToStr(LCount));

  FTestSuite.Reset;
  LCount := FTestSuite.Count;
  Check(LCount = 0,
    'Count of executed Excluded test methods should be 0 but was ' + IntToStr(LCount));
end;

{ TTestMethodHasParam }

procedure TTestMethodHasParam.HasParam(const Value: Integer);
begin
  Check(True);
end;

procedure TTestMethodHasParam.NormalMethod;
begin
  Check(True);
end;

{ TTestRegisrationOfParamMethodTestCase }

procedure TTestRegisrationOfParamMethodTestCase.SetUp;
begin
  FTestProject   := nil;
  FAnExecControl := nil;
  FTestCase      := nil;
end;

procedure TTestRegisrationOfParamMethodTestCase.TearDown;
begin
  SetUp;
  RemoveProjectManager;
end;

procedure TTestRegisrationOfParamMethodTestCase.VerifyTestCaseDoesNotListParamaterizedTestMethods;
var
  LCount: Integer;
begin
  FTestCase := TTestMethodHasParam.Create;
  Check(Assigned(FTestCase), 'TTestMethodHasParam failed to create');
  LCount := FTestCase.Count;
  Check(LCount = 1,
    'Count of tests methods should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestRegisrationOfParamMethodTestCase.VerifyTestCaseOnlyRegistersNormalTestMethods;
var
  LCount: Integer;
begin
  TestFramework.RegisterTest(TTestMethodHasParam.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Project should be created');
  LCount := FTestProject.Count;
  Check(LCount = 1,
    'Count of tests should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestRegisrationOfParamMethodTestCase.VerifyTestCaseOnlyRunsNormalTestMethods;
var
  LCount: Integer;
  LResult: TExecutionStatus;
begin
  TestFramework.RegisterTest(TTestMethodHasParam.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Project should be created');
  FExecControl := TestFramework.Projects.ExecutionControl;
  LResult := FTestProject.Run(FExecControl);
  Check(LResult = _Passed,
    'Execution result should be _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LResult)));
  LCount := FExecControl.ExecutionCount;
  Check(LCount = 1,
    'Count of executed tests should be 1 but was ' + IntToStr(LCount));
end;

{ TTestTTestITestSuiteAddsTestCases }

procedure TTestTTestITestSuiteAddsTestCases.SetUp;
begin
  FTestSuite := nil;
end;

procedure TTestTTestITestSuiteAddsTestCases.TearDown;
begin
  FTestSuite := nil;
end;

procedure TTestTTestITestSuiteAddsTestCases.TestTestSuiteCreatesOnEmptyParams;
var
  LCount: Integer;
  LTestCase: ITestCase;
begin
  LTestCase := nil;
  FTestSuite := TestFramework.TTestSuite.Suite('', LTestCase);
  Check(Assigned(FTestSuite), 'TestSuite failed to create on empty params');
  CheckEqualsString('TTestSuite', FTestSuite.DisplayedName, 'TestSuite name should be TTestSuite');
  LCount := FTestSuite.Count;
  Check(LCount = 0, 'Count of added tests should be zero but was ' + IntToStr(LCount));
end;

procedure TTestTTestITestSuiteAddsTestCases.TestTestSuiteCreatesOnNilTestCase;
var
  LCount: Integer;
  LTestCase: ITestCase;
begin
  LTestCase := nil;
  FTestSuite := TestFramework.TTestSuite.Suite('ABC', LTestCase);
  Check(Assigned(FTestSuite), 'TestSuite failed to create on empty params');
  CheckEqualsString('ABC', FTestSuite.DisplayedName, 'TestSuite name should be ABC');
  LCount := FTestSuite.Count;
  Check(LCount = 0, 'Count of added tests should be zero but was ' + IntToStr(LCount));
end;

procedure TTestTTestITestSuiteAddsTestCases.TestTestSuiteCreatesOnEmptyName;
var
  LCount: Integer;
begin
  FTestSuite := TestFramework.TTestSuite.Suite('', TTestCase1.Suite);
  Check(Assigned(FTestSuite), 'TestSuite failed to create on empty params');
  CheckEqualsString('TTestSuite', FTestSuite.DisplayedName, 'TestSuite name should be TTestSuite');
  LCount := FTestSuite.Count;
  Check(LCount = 1, 'Count of added tests should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestTTestITestSuiteAddsTestCases.TestTestSuiteCreatesOnOneTestCase;
var
  LCount: Integer;
  LTest: ITest;
begin
  FTestSuite := TestFramework.TTestSuite.Suite('ABC', TTestCase1.Suite);
  Check(Assigned(FTestSuite), 'TestSuite failed to create on empty params');
  CheckEqualsString('ABC', FTestSuite.DisplayedName, 'TestSuite name should be ABC');
  LCount := FTestSuite.Count;
  Check(LCount = 1, 'Count of added tests should be 1 but was ' + IntToStr(LCount));
  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test method was not found');
  Check(LTest.IsTestMethod, 'Last test in iterator should be a test method');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Method name should be ATestProcedure but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test case was not found');
  Check(LTest.SupportedIfaceType = _isTestCase, 'Test in iterator should now be a test case');
  Check(LTest.DisplayedName = 'TTestCase1',
    'Method name should be TTestCase1 but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(not Assigned(LTest), 'A valid test case should not be found');
end;

procedure TTestTTestITestSuiteAddsTestCases.TestTestSuiteCreatesOnMultipleTestCases;
var
  LCount: Integer;
  LTest: ITest;
begin
  FTestSuite := TestFramework.TTestSuite.Suite('PQR', [TTestCase3.Suite, TTestCase1.Suite]);
  Check(Assigned(FTestSuite), 'TestSuite failed to create on empty params');
  CheckEqualsString(FTestSuite.DisplayedName, 'PQR', 'TestSuite name should be PQR');
  LCount := FTestSuite.Count;
  Check(LCount = 3, 'Count of added tests should be 3 but was ' + IntToStr(LCount));

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test method was not found');
  Check(LTest.IsTestMethod, 'Last test in iterator should be a test method');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Method name should be ATestProcedure but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test case was not found');
  Check(LTest.SupportedIfaceType = _isTestCase, 'Test in iterator should now be a test case');
  Check(LTest.DisplayedName = 'TTestCase1',
    'Method name should be TTestCase1 but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test method was not found');
  Check(LTest.IsTestMethod, 'Last test in iterator should be a test method');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Method name should be TestProcedure2 but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test method was not found');
  Check(LTest.IsTestMethod, 'Last test in iterator should be a test method');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Method name should be TestProcedure1 but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(Assigned(LTest), 'A valid test case was not found');
  Check(LTest.SupportedIfaceType = _isTestCase, 'Test in iterator should now be a test case');
  Check(LTest.DisplayedName = 'TTestCase3',
    'Method name should be TTestCase3 but was ' + LTest.DisplayedName);

  LTest := FTestSuite.PriorTest;
  Check(not Assigned(LTest), 'A valid test case should not be found');
end;

{ TTestITestProjectExists }

procedure TTestITestProjectExists.TearDown;
begin
  FTestProject := nil;
end;

procedure TTestITestProjectExists.TestCreates;
begin
  try
    FTestProject := TestFramework.TTestProject.Create;
    Check(Assigned(FTestProject), 'Not created or assigned');
  except
    Check(False, 'Excepetion creating TTestProject');
  end;
  Check(FTestProject.SupportedIfaceType = _isTestProject,
    'Supported interface should be _isTestProject but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestProject.SupportedIfaceType)));
  FTestProject := nil;
end;

procedure TTestITestProjectExists.TestDestroys;
begin
  FTestProject := TestFramework.TTestProject.Create;
  try
    FTestProject := nil;
    Check(FTestProject = nil, 'Failed to clear holding variable on destroy')
  except
    Check(False, 'Exception in TTestProject destructor');
  end;
end;


{ TTestTestSuiteRegistersTestCases }

procedure TTestITestSuiteRegistersTestCases.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestITestSuiteRegistersTestCases.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersSingleTestCase;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase3.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');
  Check(FTestProject.Count = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestProject.Count ));
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TTestCase3','Wrong name returned');
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure1','Wrong name returned');
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure2','Wrong name returned');
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersSingleTestSuite;
var
  LTest: ITest;
  LCount: Integer;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('ABCDEF', [TTestCase3.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Test not Assigned');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestProject.Count ));
  Check(LTest.DisplayedName = 'ABCDEF','Wrong name returned');
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3','Wrong name returned');
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure1','Wrong name returned');
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure2','Wrong name returned');
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersNamedSingleTestCase;
var
  LTest: ITest;
  LCount: Integer;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('JKLMNO', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(FTestProject.Count ));
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'JKLMNO','Wrong name returned');
  Check(Supports(LTest, ITestSuite), 'Named Tests must be TTestSuites');
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersTwoTestSuites;
var
  LExeName: string;
begin
  TestTestSuiteRegistersSingleTestCase; // This loads up the first of two
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');

  TestFramework.RegisterTests('', [TTestCase2.Suite]);
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');
  Check(FTestProject.FindFirstTest <> nil, 'FindFirstTest returned nil');
  Check(FTestProject.Count = 3, 'Count <> 3 Count = ' +
    IntToStr(FTestProject.Count ));
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersTwoTestCases;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase2.Suite, TTestCase3.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');
  Check(FTestProject.Count = 3, 'Count <> 3 Count = ' +
    IntToStr(FTestProject.Count ));

  LTest := FTestProject.FindFirstTest;
  Check(Supports(LTest, ITestCase), 'Unnamed tests must be TTestCases');
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersDifferentNamedTwoTestSuites;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('ABCDEF', TTestCase2.Suite);
  TestFramework.RegisterTest('JKLMNO', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);
  Check(not FTestProject.IsTestMethod,
    'Should not show as executable Method');
  Check(FTestProject.Count = 3, 'Count <> 3 Count = ' +
    IntToStr(FTestProject.Count ));
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'ABCDEF','Wrong name returned');
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');
end;

procedure TTestITestSuiteRegistersTestCases.TestTestSuiteRegistersSameNamedTwoTestSuites;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('ABCDEF', TTestCase2.Suite);
  TestFramework.RegisterTest('ABCDEF', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  Check(FTestProject.Count = 3, 'Count <> 3 Count = ' +
    IntToStr(FTestProject.Count ));
  Check(LTest.DisplayedName = 'ABCDEF','Wrong name returned');
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');
end;

{ TTestITestProjectRegister }

procedure TTestTestProjectBehaviorOnRegisterFirstTestCase.SetUp;
begin
  inherited;
  FTestProject := nil;
end;

procedure TTestTestProjectBehaviorOnRegisterFirstTestCase.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestProjectBehaviorOnRegisterFirstTestCase.VerifyTestProjectCreatedOnRegisterFirstTestCase;
var
  LTestCase: ITestCase;
begin
  LTestCase := nil;
  Check(FTestProject = nil, 'Should be nil before starting tests');
  TestFramework.RegisterTest(LTestCase);
  Check(TestFramework.TestProject = nil, 'Should be nil after calling with nil tests');
  TestFramework.RegisterTest(TestFramework.TTestCase.Suite);
  Check(TestFramework.TestProject <> nil, 'Should not be nil after calling with valid test suite');
end;

procedure TTestTestProjectBehaviorOnRegisterFirstTestCase.VerifyTestProjectIsNamedOnRegisterFirstTestCase;
var
  LExeName: string;
begin
  Check(FTestProject = nil, 'Should be nil before starting tests');
  TestFramework.RegisterTest(TestFramework.TTestCase.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test should have registered');
  Check(FTestProject.DisplayedName <> '', 'Displayed name should not be empty string');
  LExeName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LExeName,
    'TestProject name should be ' + LExeName + ' but was ' + FTestProject.DisplayedName);
end;

{ TTestTestProjectIterator }

procedure TTestITestProjectIterator.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestITestProjectIterator.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestITestProjectIterator.TTestIterateOverSingleTestCase;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('MyTestCase', [TTestCase3.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.Count = 2, 'Count should be 2 but was ' +
    IntToStr(FTestProject.Count ));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName =
    'MyTestCase', 'TestName should be MyTestCase but was = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase3', 'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure1', 'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure2', 'TestName = ' + LTest.DisplayedName);
end;

procedure TTestITestProjectIterator.TTestIterateOverSingleRegisteredTestCase;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('MyTestCase', [TTestCase2.Suite, TTestCase3.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count should be 3 but was ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'MyTestCase',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase2',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase3',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'TestName = ' + LTest.DisplayedName);
end;

procedure TTestITestProjectIterator.TTestIterateOverTwoTestCases;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('MyTestCase', [TTestCase2.Suite, TTestCase3.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'MyTestCase',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase2',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase3',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'TestName = ' + LTest.DisplayedName);
end;

procedure TTestITestProjectIterator.TTestIterateOverThreeTestSuites;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('MyTestCase1', [TTestCase2.Suite]);
  TestFramework.RegisterTests('MyTestCase2', [TTestCase3.Suite]);
  TestFramework.RegisterTests('MyTestCase3', [TTest3TTestCase.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'MyTestCase1',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase2',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'MyTestCase2',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase3',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'MyTestCase3',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTest3TTestCase',
    'TestName = ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TestTestProcsRan',
    'TestName = ' + LTest.DisplayedName);
end;

{ TTestTestSuiteParentPathPropagates }

procedure TTestTestSuiteParentPathPropagates.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestTestSuiteParentPathPropagates.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestSuiteParentPathPropagates.TestUnNamedTestSuitePath;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase1.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.Count = 1, 'Count <> 1 Count = ' +
    IntToStr(FTestProject.Count ));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'TTestCase1',
    'TestName should be TTestCase1 but is = ' + LTest.DisplayedName);

  Check(LTest.ParentPath = FTestProject.DisplayedName,
    'Test ParentPath should be <' + FTestProject.DisplayedName + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'ATestProcedure', 'TestName = ' + LTest.DisplayedName);

  LExeName := ExtractFileName(ParamStr(0));
  Check(LTest.ParentPath = LExeName + '.TTestCase1',
    'Test procedure name should be <' + LExeName + '.TTestCase1> but is <' +
      LTest.ParentPath + '>');
end;

procedure TTestTestSuiteParentPathPropagates.TestNamedTestSuitePath;
var
  LTest: ITest;
  LExeName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('NamedSuite', [TTestCase1.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.Count = 1, 'Count <> 1 Count = ' +
    IntToStr(FTestProject.Count ));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'NamedSuite',
    'TestName should be NamedSuite but is = ' + LTest.DisplayedName);

  Check(LTest.ParentPath = FTestProject.DisplayedName,
    'Test ParentPath should be <' + FTestProject.DisplayedName  + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase1',
    'TestName should be TTestCase1 but is = ' + LTest.DisplayedName);

  Check(LTest.ParentPath = FTestProject.DisplayedName + '.NamedSuite',
    'Test ParentPath should be <' + FTestProject.DisplayedName  + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Cannot fetch ITest');
  Check(LTest.DisplayedName = 'ATestProcedure', 'TestName = ' + LTest.DisplayedName);

  LExeName := ExtractFileName(ParamStr(0));
  Check(LTest.ParentPath = LExeName + '.NamedSuite.TTestCase1',
    'Test procedure name should be <' + LExeName + '.NamedSuite.TTestCase1> but is <' +
      LTest.ParentPath + '>');
end;

procedure TTestTestSuiteParentPathPropagates.TestChangedNamedPropogatesParentPath;
var
  LTest: ITest;
  LExeName: string;
begin
  //Setup and test a path
  TestNamedTestSuitePath;
  // Now change the top level name and ensure subordinate ITest's paths change.
  LExeName := 'Changed';
  FTestProject.DisplayedName := LExeName;
  LTest := FTestProject.FindFirstTest;

  Check(LTest.ParentPath = LExeName,
    'Test ParentPath should be <' + LExeName + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(LTest.ParentPath = LExeName + '.NamedSuite',
    'Test ParentPath should be <' + LExeName  + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(LTest.ParentPath = LExeName + '.NamedSuite.TTestCase1',
    'Test procedure name should be <' + LExeName + '.NamedSuite.TTestCase1> but is <' +
      LTest.ParentPath + '>');
end;

procedure TTestTestSuiteParentPathPropagates.TestChangedParentPathPropogatesParentPath;
var
  LTest: ITest;
  LExeName: string;
begin
  //Setup and test a path
  TestNamedTestSuitePath;
  // Now change the top level name and ensure subordinate ITest's paths change.
  LExeName := 'Changed';
  LTest := FTestProject.FindFirstTest;
  LTest.ParentPath := LExeName;

  Check(LTest.ParentPath = LExeName,
    'Test ParentPath should be <' + LExeName  + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(LTest.ParentPath = LExeName + '.NamedSuite',
    'Test ParentPath should be <' + LExeName  + '> but is <' +
      LTest.ParentPath + '>');

  LTest := FTestProject.FindNextTest;
  Check(LTest.ParentPath = LExeName + '.NamedSuite.TTestCase1',
    'Test procedure name should be <' + LExeName + '.NamedSuite.TTestCase1> but is <' +
      LTest.ParentPath + '>');
end;


{ TTestITestCanRunProject }

procedure TTestITestCanRunProject.ExecStatusCallBack1(const ATest: ITest);
var
  LStatusRecord: TStatusRecord;
begin
  if (ATest <> nil) and Supports(ATest, ITest) then
  begin
    SetLength(FStatusArray, Length(FStatusArray)+1);
    LStatusRecord.ExecStatus := (ATest as ITest).ExecStatus;
    LStatusRecord.AName := (ATest as ITest).DisplayedName;
    FStatusArray[Length(FStatusArray)-1] := LStatusRecord;
  end;
end;

procedure TTestITestCanRunProject.ExecStatusCallBack2(const ATest: ITest);
var
  LExecStatus: TExecutionStatus;
begin
  if (ATest <> nil) and Supports(ATest, ITest) and FSecondPass then
  begin
    LExecStatus := ATest.ExecStatus;
    if LExecStatus = _Ready then
      Inc(FReadyCount);
  end;
end;

procedure TTestITestCanRunProject.SetUp;
begin
  FTest := nil;
  FAnExecControl := nil;
  FReadyCount := 0;
  FTestProject := nil;
  SetLength(FStatusArray,0);
  FList := TInterfaceList.Create;
end;

procedure TTestITestCanRunProject.TearDown;
begin
  FList := nil;
  FTest := nil;
  FAnExecControl := nil;
  SetLength(FStatusArray,0);
  FTestProject := nil;
  RemoveProjectManager;
end;

function TTestITestCanRunProject.ShouldRunTest(const ATest: ITest): boolean;
begin
  Result := False;
  if ATest = nil then
    Exit;
  if not Assigned(FList) then
    Exit;
  Result := FList.IndexOf(ATest) >= 0;
end;

procedure TTestITestCanRunProject.TestCanRunOneTestProc;
var
  LTest: ITest;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase2.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(LExecControl); //Resets TestIterator

  LTest:= FTestProject.FindFirstTest;
  Check((LTest as ITestTestCase2).Get_SetupRan, 'Setup failed to Run');
  Check((LTest as ITestTestCase2).Get_RunRan,   'Run failed to Run');
  Check((LTest as ITestTestCase2).Get_TearDownRan, 'TearDown failed to Run');
  Check((LTest as ITestTestCase2).Get_ATestProcRan, 'TestProc failed to Run');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'ATestProcedure', 'Wrong test, name = ' +
    LTest.DisplayedName);
end;

procedure TTestITestCanRunProject.TestCanIndividuallyRunOneTestProc;
var
  LTest: ITest;
  LCount: Integer;
  ReturnStatus: TExecutionStatus;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase3.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest:= FTestProject.FindFirstTest;
  FList.Add(LTest);
  FTest := FTestProject.FindNextEnabledProc;
  FTest := FTestProject.FindNextEnabledProc;
  FList.Add(FTest);

  Check(Assigned(FTest), 'FTest should be Assigned');
  Check(FTest.DisplayedName = 'TestProcedure2',
    'FTest should have TestProcedure2 but was ' + FTest.DisplayedName);
  FAnExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FAnExecControl.IndividuallyEnabledTest := ShouldRunTest;

  ReturnStatus := FTestProject.Run(FAnExecControl);
  FList.Clear;
  Check(ReturnStatus = _Warning,
    'Individually enabled test (As executed from GUI by F8) returned value should be _Warning but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(ReturnStatus)));

  LTest:= FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'LTest should be Assigned');
  LCount := (LTest as ITestTestCase3).Get_TestProcRan;
  // The first of the two procedures should not have run
  Check(LCount = 2, 'Return value should be 2 but was ' + IntToStr(LCount));
end;

procedure TTestITestCanRunProject.TestCanRunTwoTestProcs;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCase3.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure1', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure2', 'Wrong test, name = ' +
    LTest.DisplayedName);

  FAnExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(FAnExecControl);
  FTestProject.Reset;
  LCount := -1;
  LTest := FTestProject.FindFirstTest;

  if LTest.DisplayedName = 'TTestCase3' then
    LCount := (LTest as ITestTestCase3).Get_TestProcRan;
  Check(LCount = 3, 'Failed to Run');
end;

procedure TTestITestCanRunProject.TestCanRunTwoSuites;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.RegisterTest(TTestCase3.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Neither of two TestCases registered');

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));
  Check(Assigned(LTest), 'Could not locate a Test');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Should be TTestCase2 but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test');
  Check(LTest.DisplayedName = 'ATestProcedure', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3',
    'Should be TTestCase3 but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test');
  Check(LTest.DisplayedName = 'TestProcedure1', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test');
  Check(LTest.DisplayedName = 'TestProcedure2', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'ATestProcedure', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure1', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure2', 'Wrong test, name = ' +
    LTest.DisplayedName);

  FAnExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(FAnExecControl);

  LTest := FTestProject.FindFirstTest;
  Check((LTest as ITestTestCase2).Get_ATestProcRan,
    'First procedure did not execute');

  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LCount := (LTest as ITestTestCase3).Get_TestProcRan;
  Check(LCount = 3, '2nd or 3rd procedure did not execute');
end;

procedure TTestTestProjectAddMethods.TearDown;
begin
  FSuite := nil;
  FTestSuite := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestProjectAddMethods.TestCheckCountEnabledTests;
var
  LCount: Integer;
  LTest: ITest;
begin
  FTestProject := TestFramework.TTestProject.Create;
  FTestProject.AddTest(TTestCase3.Create);
  Check(FTestProject.DisplayedName = 'TTestProject',
    'TestProject name should be TTestProject but was ' + FTestProject.DisplayedName);
  LCount := FTestProject.Count;
  Check(LCount = 2, 'TestProject`s test method count should be 2 but was ' + IntToStr(LCount));

  LCount := FTestProject.CountEnabledTests;
  Check(LCount = 2, 'TestProject`s test method count should be 2 but was ' + IntToStr(LCount));

  // Now disable the test methods and do a recount;
  LCount := 0;
  FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  while Assigned(LTest) do
  begin
    if LTest.IsTestMethod then
    begin
      LTest.Enabled := False;
      Inc(LCount);
    end;
    LTest := FTestProject.FindNextTest;
  end;
  Check(LCount = 2, 'Should have disabled 2 test methods but only did' + IntToStr(LCount));
  FTestProject.Reset;  // I don't like the fact that it's necessary to reset
  LCount := FTestProject.Count;
  Check(LCount = 0, 'Count should now be 0 but was' + IntToStr(LCount));
end;

{ TTestTestProjectAddSuite }

procedure TTestTestProjectAddSuite.TearDown;
begin
  FSuite := nil;
  FTestSuite := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestProjectAddSuite.TestCheckNameOnAddSuite;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TTestProject.Create;
  FTestProject.AddTest(TTestCase3.Create);
  Check(FTestProject.DisplayedName = 'TTestProject',
    'TestProject name should be TTestProject but was ' + FTestProject.DisplayedName);
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TTestCase3', 'Name should be TTestProject but was ' + LTest.DisplayedName);
end;

{ TTestCase4 }

procedure TTestCase4.TestProcedure1;
begin
  Inc(FTestProcRan ,1);
end;

procedure TTestCase4.TestProcedure2;
begin
  Inc(FTestProcRan ,2);
  Assert(1=0,' Must fail here');
end;

procedure TTestCase4.TestProcedure3;
begin
  Inc(FTestProcRan ,4);
end;

procedure TTestITestCanRunProject.TestMassClearStatus;
begin
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.RegisterTest(TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBack2;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(FAnExecControl); //Sets up no nonready status
  FSecondPass := True;
  FTestProject.Run(FAnExecControl); //Sets up no nonready status
  Check(FReadyCount = 5, // TTestCase2 + 1 + TTestCase4 + 2 = 5
    '_Ready Status should be reported 5 times but was ' + IntToStr(FReadyCount));
end;

procedure TTestITestCanRunProject.TestStatusCallBack;
var
  LCount: Integer;
  LExeName: string;
begin
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.RegisterTest(TTestCase4.Suite);
  FTestProject := TestFramework.TestProject;
  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBack1;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := True;
  {$ENDIF}
  FTestProject.Run(FAnExecControl);
  LCount := Length(FStatusArray);
  Check(LCount > 0, 'StatusListner not called');
  Check(LCount = 14, 'Wrong Count, should be 14 but was ' + IntToStr(LCount));
  // The following is the mimimalist reporting scenario.
  // Only changes that need reporting should generate a callback.

  LExeName := ExtractFileName(ParamStr(0));
  Check(FStatusArray[ 0].AName = LExeName        , FStatusArray[ 0].AName  + '  0 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 0].ExecStatus)));
  Check(FStatusArray[ 1].AName = 'TTestCase2'    , FStatusArray[ 1].AName  + '  1 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 1].ExecStatus)));
  Check(FStatusArray[ 2].AName = 'ATestProcedure', FStatusArray[ 2].AName  + '  2 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 2].ExecStatus)));
  Check(FStatusArray[ 3].AName = 'ATestProcedure', FStatusArray[ 3].AName  + '  3 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 3].ExecStatus)));
  Check(FStatusArray[ 4].AName = 'TTestCase2'    , FStatusArray[ 4].AName  + '  4 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 4].ExecStatus)));
  Check(FStatusArray[ 5].AName = 'TTestCase4'    , FStatusArray[ 5].AName  + '  5 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 5].ExecStatus)));
  Check(FStatusArray[ 6].AName = 'TestProcedure1', FStatusArray[ 6].AName  + '  6 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 6].ExecStatus)));
  Check(FStatusArray[ 7].AName = 'TestProcedure1', FStatusArray[ 7].AName  + '  7 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 7].ExecStatus)));
  Check(FStatusArray[ 8].AName = 'TestProcedure2', FStatusArray[ 8].AName  + '  8 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 8].ExecStatus)));
  Check(FStatusArray[ 9].AName = 'TestProcedure2', FStatusArray[ 9].AName  + '  9 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[ 9].ExecStatus)));
  Check(FStatusArray[10].AName = 'TestProcedure3', FStatusArray[10].AName  + ' 10 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[10].ExecStatus)));
  Check(FStatusArray[11].AName = 'TestProcedure3', FStatusArray[11].AName  + ' 11 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[11].ExecStatus)));
  Check(FStatusArray[12].AName = 'TTestCase4'    , FStatusArray[12].AName  + ' 12 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[12].ExecStatus)));
  Check(FStatusArray[13].AName = LExeName        , FStatusArray[13].AName  + ' 13 ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(FStatusArray[13].ExecStatus)));

  Check(FStatusArray[ 0].ExecStatus = _Running,  LExeName       + ' not notified of Running state');
  Check(FStatusArray[ 1].ExecStatus = _Running, 'TTestCase2     not notified of Running state');
  Check(FStatusArray[ 2].ExecStatus = _Running, 'ATestProcedure not notified of Running state');
  Check(FStatusArray[ 3].ExecStatus = _Warning, 'ATestProcedure not notified of _Warning  state');
  Check(FStatusArray[ 4].ExecStatus = _Warning, 'TTestCase2     not notified of _Warning  state');
  Check(FStatusArray[ 5].ExecStatus = _Running, 'TTestCase4     not notified of Running state');
  Check(FStatusArray[ 6].ExecStatus = _Running, 'TestProcedure1 not notified of Running state');
  Check(FStatusArray[ 7].ExecStatus = _Warning, 'TestProcedure1 not notified of _Warning  state');
  Check(FStatusArray[ 8].ExecStatus = _Running, 'TestProcedure2 not notified of _Running  state');
  Check(FStatusArray[ 9].ExecStatus = _Error  , 'TestProcedure2 not notified of Error   state');
  Check(FStatusArray[10].ExecStatus = _Running, 'TestProcedure3 not notified of Running state');
  Check(FStatusArray[11].ExecStatus = _Warning, 'TestProcedure3 not notified of Warning state');
  Check(FStatusArray[12].ExecStatus = _Error  , 'TTestCase4     not notified of Error   state');
  Check(FStatusArray[13].ExecStatus = _Error  , LExeName     + 'not notified of _Warning  state');
end;

procedure TTestITestCanRunProject.TestPMLevelSingleProjectStatusCallBack;
var
  LCount: Integer;
  LExeName: string;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TTestCase2.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Test Project should exist');
  LExecControl := TestFramework.Projects.ExecutionControl;
  LExecControl.ExecStatusUpdater := ExecStatusCallBack1;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(LExecControl);
  LCount := Length(FStatusArray);
  Check(LCount > 0, 'StatusListner not called');
  Check(LCount = 6, 'Wrong Count, should be 6 but was ' + IntToStr(LCount));
  // The following is the mimimalist reporting scenario.
  // Only changes that need reporting generate a callback.

  LExeName := ExtractFileName(ParamStr(0));
  Check(FStatusArray[ 0].AName = LExeName        , FStatusArray[ 0].AName);
  Check(FStatusArray[ 1].AName = 'TTestCase2'    , FStatusArray[ 1].AName);
  Check(FStatusArray[ 2].AName = 'ATestProcedure', FStatusArray[ 2].AName);

  Check(FStatusArray[ 3].AName = 'ATestProcedure', FStatusArray[ 3].AName);
  Check(FStatusArray[ 4].AName = 'TTestCase2'    , FStatusArray[ 4].AName);
  Check(FStatusArray[ 5].AName = LExeName        , FStatusArray[ 5].AName);

  Check(FStatusArray[ 0].ExecStatus = _Running,  LExeName + ' not notified of Running state');
  Check(FStatusArray[ 1].ExecStatus = _Running, 'TTestCase2     not notified of Running state');
  Check(FStatusArray[ 2].ExecStatus = _Running, 'ATestProcedure not notified of Running state');

  Check(FStatusArray[ 3].ExecStatus = _Warning, 'ATestProcedure not notified of _Warning  state');
  Check(FStatusArray[ 4].ExecStatus = _Warning, 'TTestCase2     not notified of _Warning  state');
  Check(FStatusArray[ 5].ExecStatus = _Warning,  LExeName + ' not notified of _Warning  state');

  Check(FStatusArray[0].AName = LExeName, 'First updated should be ' + LExeName + ' but was ' + FStatusArray[0].AName);
  Check(FStatusArray[LCount-1].AName = LExeName, 'Last updated should be ' + LExeName + ' but was ' + FStatusArray[LCount-1].AName);
end;


{ TTestCaseWithPassAndFailure }

procedure TPassAndFail.TestProcPasses;
begin
  Check(True);
end;

procedure TPassAndFail.TestProcFails;
begin
  Check(False, 'Deliberate failure');
end;

procedure TTestITestCanRunProject.TestPMLevelMultiProjectStatusCallBack;
var
  LCount: Integer;
  LExeName: string;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('AnotherProject', TPassAndFail.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Test Project should exist');
  LExecControl := TestFramework.Projects.ExecutionControl;
  LExecControl.ExecStatusUpdater := ExecStatusCallBack1;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(LExecControl);
  LCount := Length(FStatusArray);
  Check(LCount > 0, 'StatusListner not called');
  Check(LCount = 16, 'Wrong Count, should be 16 but was ' + IntToStr(LCount));
  // The following is the mimimalist reporting scenario.
  // Only changes that need reporting generate a callback.

  LExeName := ExtractFileName(ParamStr(0));
  Check(FStatusArray[0].AName = LExeName, 'First updated should be ' + LExeName + ' but was ' + FStatusArray[0].AName);
  Check(FStatusArray[LCount-1].AName = LExeName, 'Last updated should be ' + LExeName + ' but was ' + FStatusArray[LCount-1].AName);

  Check(FStatusArray[ 0].AName = LExeName        , 'FStatusArray[ 0] = ' + FStatusArray[ 0].AName);
  Check(FStatusArray[ 1].AName = DefaultProject  , 'FStatusArray[ 1] = ' + FStatusArray[ 1].AName);
  Check(FStatusArray[ 2].AName = 'TTestCase2'    , 'FStatusArray[ 2] = ' + FStatusArray[ 2].AName);
  Check(FStatusArray[ 3].AName = 'ATestProcedure', 'FStatusArray[ 3] = ' + FStatusArray[ 3].AName);
  Check(FStatusArray[ 4].AName = 'ATestProcedure', 'FStatusArray[ 4] = ' + FStatusArray[ 4].AName);
  Check(FStatusArray[ 5].AName = 'TTestCase2'    , 'FStatusArray[ 5] = ' + FStatusArray[ 5].AName);
  Check(FStatusArray[ 6].AName = DefaultProject  , 'FStatusArray[ 6] = ' + FStatusArray[ 6].AName);
  Check(FStatusArray[ 7].AName = 'AnotherProject', 'FStatusArray[ 7] = ' + FStatusArray[ 7].AName);
  Check(FStatusArray[ 8].AName = 'TPassAndFail'  , 'FStatusArray[ 8] = ' + FStatusArray[ 8].AName);
  Check(FStatusArray[ 9].AName = 'TestProcPasses', 'FStatusArray[ 9] = ' + FStatusArray[ 9].AName);
  Check(FStatusArray[10].AName = 'TestProcPasses', 'FStatusArray[10] = ' + FStatusArray[10].AName);
  Check(FStatusArray[11].AName = 'TestProcFails' , 'FStatusArray[11] = ' + FStatusArray[11].AName);
  Check(FStatusArray[12].AName = 'TestProcFails' , 'FStatusArray[12] = ' + FStatusArray[12].AName);
  Check(FStatusArray[13].AName = 'TPassAndFail'  , 'FStatusArray[13] = ' + FStatusArray[13].AName);
  Check(FStatusArray[14].AName = 'AnotherProject', 'FStatusArray[14] = ' + FStatusArray[14].AName);
  Check(FStatusArray[15].AName = LExeName        , 'FStatusArray[15] = ' + FStatusArray[15].AName);

  Check(FStatusArray[ 0].ExecStatus = _Running, LExeName         + ' not notified of Running state');
  Check(FStatusArray[ 1].ExecStatus = _Running, DefaultProject   + ' not notified of Running state');
  Check(FStatusArray[ 2].ExecStatus = _Running, 'TTestCase2'     + ' not notified of _Warning  state');
  Check(FStatusArray[ 3].ExecStatus = _Running, 'ATestProcedure' + ' not notified of _Warning  state');
  Check(FStatusArray[ 4].ExecStatus = _Warning, 'ATestProcedure' + ' not notified of _Warning  state');
  Check(FStatusArray[ 5].ExecStatus = _Warning, 'TTestCase2'     + ' not notified of _Warning state');
  Check(FStatusArray[ 6].ExecStatus = _Warning, DefaultProject   + ' not notified of _Warning state');
  Check(FStatusArray[ 7].ExecStatus = _Running, 'AnotherProject' + ' not notified of Running state');
  Check(FStatusArray[ 8].ExecStatus = _Running, 'TPassAndFail'   + ' not notified of Running state');
  Check(FStatusArray[ 9].ExecStatus = _Running, 'TestProcPasses' + ' not notified of Running state');
  Check(FStatusArray[10].ExecStatus = _Passed,  'TestProcPasses' + ' not notified of _Passed state');
  Check(FStatusArray[11].ExecStatus = _Running, 'TestProcFails'  + ' not notified of _Running state');
  Check(FStatusArray[12].ExecStatus = _Failed,  'TestProcFails'  + ' not notified of _Failed state');
  Check(FStatusArray[13].ExecStatus = _Failed,  'TPassAndFail'   + ' not notified of _Failed state');
  Check(FStatusArray[14].ExecStatus = _Failed,  'AnotherProject' + ' not notified of _Failed state');
  Check(FStatusArray[15].ExecStatus = _Failed,  LExeName         + ' not notified of _Failed state');
  end;

{ TTestMultiProjectCount }

procedure TTestMultiProjectCount.SetUp;
begin
  FAnExecControl := nil;
  FReadyCount := 0;
  FTestProject := nil;
end;

procedure TTestMultiProjectCount.TearDown;
begin
  FTest := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestMultiProjectCount.TestPMLevelSingleProjectCounts;
var
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TTestCase3.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Test Project should exist');
  LExecControl := TestFramework.Projects.ExecutionControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  LCount := TestFramework.Projects.Count;
  Check(LCount = 2,
    'Projects Count should be 2 but is ' + IntToStr(LCount));
  FTestProject.Run(LExecControl);

  Check(LExecControl.EnabledCount = 2,
    'Enabled Count should be 2 but is ' + IntToStr(LExecControl.EnabledCount));
  Check(LExecControl.ExecutionCount = 2,
    'Execution Count should be 2 but is ' + IntToStr(LExecControl.ExecutionCount));
  Check(LExecControl.WarningCount = 2,
    'Warning Count should be 2 but is ' + IntToStr(LExecControl.WarningCount));
  Check(LExecControl.ErrorCount = 0,
    'Error Count should be 0 but is ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.FailureCount = 0,
    'Failure Count should be 0 but is ' + IntToStr(LExecControl.FailureCount));
end;

procedure TTestMultiProjectCount.TestPMLevelMultiProjectCounts;
var
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TTestCase3.Suite);
  TestFramework.ProjectRegisterTest('AnotherProject', TPassAndFail.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Test Project should exist');
  LExecControl := TestFramework.Projects.ExecutionControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  LCount := TestFramework.Projects.Count;
  Check(LCount = 4,
    'Projects Count should be 1 but is ' + IntToStr(LCount));
  FTestProject.Run(LExecControl);

  Check(LExecControl.EnabledCount = 4,
    'Enabled Count should be 4 but is ' + IntToStr(LExecControl.EnabledCount));
  Check(LExecControl.ExecutionCount = 4,
    'Execution Count should be 4 but is ' + IntToStr(LExecControl.ExecutionCount));
  Check(LExecControl.WarningCount = 2,
    'Warning Count should be 2 but is ' + IntToStr(LExecControl.WarningCount));
  Check(LExecControl.ErrorCount = 0,
    'Error Count should be 0 but is ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.FailureCount = 1,
    'Failure Count should be 1 but is ' + IntToStr(LExecControl.FailureCount));
end;

{ TExceptTestCase }

procedure TExceptTestCase.ATestExcepts;
var
  e: EAbort;
begin
  e := EABort.Create('Deliberate exception to test handling');
  raise(e);
end;

{ TTestTestCase1 }

procedure TTestTestCase1.Setup;
begin
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestCase1 := TestFramework.TTestCase.Create;
  Check(FTestCase1.SupportedIfaceType = _isTestCase,
    'Supported interface should be _isTestCase but was ' +
      GetEnumName(TypeInfo(TSupportedIface ), Ord(FTestCase1.SupportedIfaceType)));
  FExceptOnRun := TExceptTestCase.Create;
  FTestCase1.AddTest(FExceptOnRun);
  FTestCase1.Count;
end;

procedure TTestTestCase1.TearDown;
begin
  FAnExecControl := nil;
  FTestCase1 := nil;
  FExceptOnRun := nil;
end;

procedure TTestTestCase1.TestTestExceptionInRun;
begin
  FTestCase1.Run(FAnExecControl);
  Check(True);
end;

{ TTestCallToFail }

procedure TTestCallToFail.TestCallFail;
begin
  (Self as ITestMethod).Fail('Deliberate Fail',
    {$IFDEF CLR} nil {$ELSE} MethodAddress('TestCallFail') {$ENDIF});
end;

procedure TTestCallToFail.TestCallFailEquals;
begin
  (Self as ITestMethod).FailEquals('ThisString','ThisString',
    'Deliberate Fail on Equal',
    {$IFDEF CLR} nil {$ELSE} MethodAddress('TestCallFail') {$ENDIF});
end;

procedure TTestCallToFail.TestCallFailNotEquals;
begin
  (Self as ITestMethod).FailNotEquals('IsNotEqual','ToThis',
    'Deliberate Fail on NotEquals',
      {$IFDEF CLR} nil {$ELSE} MethodAddress('TestCallFail') {$ENDIF});
end;

procedure TTestCallToFail.TestCallFailNotSame;
begin
  (Self as ITestMethod).FailNotSame('IsNotSame','AsThis',
    'Deliberate Fail on NotSame',
      {$IFDEF CLR} nil {$ELSE} MethodAddress('TestCallFail') {$ENDIF});
end;


{ TTestTestCaseHandlesFailCalls }

procedure TTestTestCaseHandlesFailCalls.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestTestCaseHandlesFailCalls.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestCaseHandlesFailCalls.TestITestCaseFailCalls;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCallToFail.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestCallFail', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestCallFailEquals', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestCallFailNotEquals', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestCallFailNotSame', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  Check(LTest.ExecStatus = _Failed, ' Test should have failed');

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestCallToHalt }

procedure TTestCallToHalt.TestPass;
var
  A,B : Integer;
begin
  A := 1;
  B := 2;
  Assert((A+B)=3, 'Just coded enough to evert empty test fail');
end;

procedure TTestCallToHalt.TestHalts;
begin
  StopTests('Testing deliberatley halted');
end;

procedure TTestCallToHalt.TestFails;
begin
  (Self as ITestMethod).Fail('Deliberate Fail',
    {$IFDEF CLR} nil {$ELSE} MethodAddress('TestCallFail') {$ENDIF});
end;

{ TTestTestCaseHandlesHaltCall }

procedure TTestTestCaseHandlesHaltCall.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestTestCaseHandlesHaltCall.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestCaseHandlesHaltCall.TestITestCaseHalts;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCallToHalt.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestPass', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestHalts', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestFails', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Warning, 'Suppressed empty test should just be _Warning');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Stopped, 'Test should have failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Ready, 'Test should not have executed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestHandlesBreakOnNotPassed }

procedure TTestHandlesBreakOnNotPassed.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestHandlesBreakOnNotPassed.TearDown;
begin
  FAnExecControl.ExecStatusUpdater := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  TestFramework.UnRegisterProjectManager;
end;

procedure TTestHandlesBreakOnNotPassed.VerifyTestsBreakOnFailure;
var
  LCount: Integer;
  LStatus: TExecutionStatus;
begin
  TestFramework.RegisterTest(TMiddleOfThreeTestsFail.Suite);
  FTestProject := TestFramework.Projects;

  Check(Assigned(FTestProject), 'Project must now exist');
  FAnExecControl := FTestProject.ExecutionControl;
  // Prevent an apparent memory leak caused by stack traceing
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LCount := FTestProject.Count;

  // Validate test scenario
  Check(LCount = 3, 'Should report 3 tests but was ' + IntToStr(LCount));
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Failed,
    'Status should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 3, 'Should report 3 tests run but was ' + IntToStr(LCount));

  // Now Re-run tests with BreakOnFailures set and without clearing counts
  FAnExecControl.BreakOnFailures := True;
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Failed,
    'Status should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 5, 'Should report 5 tests run but was ' + IntToStr(LCount));

  LCount := FAnExecControl.FailureCount;
  Check(LCount = 2, 'Should report 2 test failed but was ' + IntToStr(LCount));
end;

procedure TTestHandlesBreakOnNotPassed.VerifyTestsBreakOnException;
var
  LCount: Integer;
  LStatus: TExecutionStatus;
begin
  TestFramework.RegisterTest(TMiddleOfThreeTestsExcept.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Project must now exist');
  FAnExecControl := FTestProject.ExecutionControl;
  // Prevent an apparent memory leak caused by stack traceing
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := True; //FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LCount := FTestProject.Count;
  // Validate test scenario
  Check(LCount = 3, 'Should report 3 tests but was ' + IntToStr(LCount));
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Error,
    'Status should be _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 3, 'Should report 3 tests run but was ' + IntToStr(LCount));

  // Now Re-run tests with BreakOnFailures set and without clearing counts
  FAnExecControl.BreakOnFailures := True;
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Error,
    'Status should be _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 5, 'Should report 5 tests run but was ' + IntToStr(LCount));

  LCount := FAnExecControl.ErrorCount;
  Check(LCount = 2, 'Should report 2 test Errors but was ' + IntToStr(LCount));
end;

{$IFNDEF CLR}
{ TTestEmptyTest }

{$IFOPT O-}
{$DEFINE UNOPTIMIZED}
{$OPTIMIZATION ON}
{$ENDIF}
procedure TTestEmptyTest.TestEmptyTest;
begin
// is empty
end;
{$IFDEF UNOPTIMIZED}
{$OPTIMIZATION OFF}
{$ENDIF}

{ TTestFailsOnOptimizedEmptyTest }

procedure TTestFailsOnOptimizedEmptyTest.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestFailsOnOptimizedEmptyTest.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestFailsOnOptimizedEmptyTest.TestITestEmptyTest;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestEmptyTest.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestEmptyTest', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should fail');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;
{$ENDIF}

procedure TTestCheckNotCalled.TestCheckIsCalled;
begin
  Check(True);
end;

procedure TTestCheckNotCalled.TestCheckNotCalled;
var
  A,B : Integer;
begin
  A := 1;
  B := 2;
  Assert((A+B)=3, 'Just coded enough to evert empty test fail');
end;

{ TTestFailsOnCheckNotCalled }

procedure TTestFailsOnCheckNotCalled.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestFailsOnCheckNotCalled.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestFailsOnCheckNotCalled.TestITestCheckNotCalled;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestCheckNotCalled.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestCheckNotCalled', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.FailsOnNoChecksExecuted := True;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.CheckCalled, 'First Test should have called check');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.CheckCalled = False, 'Second Test should not have called check');
  Check(LTest.ExecStatus = _Failed, 'Test should fail');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ Exception generators }

procedure TTestUnknownExceptHandled.TestUnKnownExcept;
begin
  raise EUnitTestsUnknownExcept.Create('Deliberate Unknown Exception');
end;

procedure TTestUnExpectedExceptHandled.TestUnExpectedExcept;
begin
  StartExpectingException(EUnitTestsExpectedExcept);
  raise EUnitTestsUnknownExcept.Create('Deliberate different Exception');
end;

procedure TTestExpectedExceptHandled.TestExpectedExcept;
begin
  StartExpectingException(EUnitTestsExpectedExcept);
  raise EUnitTestsExpectedExcept.Create('Deliberate expected Exception');
end;

procedure TTestExpectedExceptHandled.TestExpectedExceptClears;
begin
  Check(ExpectedException = nil, 'ExpectedException should be nil');
end;

procedure TTestMissingExpectedExcept.TestMissingExpectedExcept;
begin
  StartExpectingException(EUnitTestsExpectedExcept);
end;

procedure TTestAssertMessageHandled.TestAssert;
begin
  Assert(False, 'Assert error message');
  Check(False, 'Assert failed to fire');
end;

{ TTestExceptionHandled }
procedure TTestExceptionHandled.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestExceptionHandled.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestExceptionHandled.TestITestHandlesUnknowExcept;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestUnknownExceptHandled.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestUnKnownExcept', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestUnExpectedExceptHandled }
procedure TTestExceptionHandled.TestITestHandlesUnExpectedExcept;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestUnExpectedExceptHandled.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestUnExpectedExcept', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

procedure TTestExceptionHandled.TestITestHandlesAssert;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestAssertMessageHandled.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestAssert', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have raised _Error');
//  Check(Pos('Assert error message',LTest.ErrorMessage) = 1,
//    'Error message was ' + LTest.ErrorMessage + ' but should have contained "Assert error message"');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

procedure TTestExceptionHandled.TestITestHandlesExpectedExcept;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestExpectedExceptHandled.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestExpectedExcept', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; // This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed, 'First test should have Passed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed, 'Second test should have Passed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'List of tests should be expended');
end;

procedure TTestExceptionHandled.TestITestHandlesMissingExpectedExcept;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestMissingExpectedExcept.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestMissingExpectedExcept', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have failed with _Failed');
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{---------------------------}
{ TTestExceptInDoSetup }

procedure TTestExceptInDoSetup.SetUpOnce;
begin
  raise EAbort.Create('Deliberate exception in SetUpOnce');
end;

procedure TTestExceptInDoSetup.TestExceptInDoSetUp;
begin
  Fail('TestExceptInDoSetup should not run');
end;


{ TTestExceptInSetup }

procedure TTestExceptInSetup.Setup;
begin
  raise EAbort.Create('Deliberate exception in SetUp');
end;

procedure TTestExceptInSetup.Test1stMethodWithSetUpFailure;
begin
  Check(True, 'Would pass but must show Fail in TreeView');
end;

procedure TTestExceptInSetup.Test2ndMethodWithSetUpFailure;
begin
  Check(True, 'Would pass but must show Fail in TreeView');
end;

{ TTestExceptInTearDown }

procedure TTestExceptInTearDown.TearDown;
begin
  raise EAbort.Create('Deliberate exception in TearDown');
end;

procedure TTestExceptInTearDown.Test1stMethodWithTearDownFailure;
begin
  Check(True);
end;

procedure TTestExceptInTearDown.Test2ndMethodWithTearDownFailure;
begin
  Check(True);
end;

{ TTestExceptInDoTearDown }

procedure TTestExceptInDoTearDown.TearDownOnce;
begin
  raise EAbort.Create('Deliberate exception in TearDownOnce');
end;

procedure TTestExceptInDoTearDown.DoTestExceptInDoTearDown;
begin
  Check(True);
end;

{ THasSetupFailureOn1stMethod }

procedure THasSetupFailureOn1stMethod.SetUpOnce;
begin
  FCount := 0;
end;

procedure THasSetupFailureOn1stMethod.SetUp;
begin
  Inc(FCount);
  Check(FCount <> 1, ''); //'Causes setup to fail on first execution');
end;

procedure THasSetupFailureOn1stMethod.Method2SetUpOKandPasses;
begin
  Check(True);
end;

procedure THasSetupFailureOn1stMethod.SetUpFailsandMethod1FailsIfExecuted;
begin
  Check(False, 'Should not execute');
end;

{ TTestCheckFailInSetup }

procedure THasSetupFailureOn2ndMethod.SetUp;
begin
  Inc(FCount);
  CheckNotEquals(FCount, 2, 'Deliberate Check failure in SetUp');
end;

procedure THasSetupFailureOn2ndMethod.SetUpOnce;
begin
  FCount := 0;
end;

procedure THasSetupFailureOn2ndMethod.Method1SetUpOKandPasses;
begin
  Check(True, 'Should pass');
end;

procedure THasSetupFailureOn2ndMethod.SetUpFailsandMethod2FailsIfExecuted;
begin
  Check(False, 'This failure message should not be received');
end;

procedure THasSetupFailureOn2ndMethod.Method3SetUpOKandPasses;
begin
  Check(True, 'Should pass');
end;

{ THasTeardownFailure }

procedure THasTeardownFailure.SetUpOnce;
begin
  FCount := 0;
end;

procedure THasTeardownFailure.TearDown;
begin
  Inc(FCount);
  CheckNotEquals(FCount, 2, 'Deliberate Check failure in Teardown');
end;

procedure THasTeardownFailure.ExecutedMethodPasses;
begin
  Check(True, 'Should pass');
end;

procedure THasTearDownFailure.ExecutedMethodTeardownFails;
begin
  Check(True, 'Should pass then fail when Teardown fails');
end;

{ TTestTestCaseHandlesNonMethodFailures }

procedure TTestTestCaseHandlesNonMethodFailures.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestTestCaseHandlesNonMethodFailures.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTestCaseHandlesNonMethodFailures.CheckSetupFailure;
var
  LTest: ITest;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(THasSetupFailureOn2ndMethod.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Method1SetUpOKandPasses', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SetUpFailsandMethod2FailsIfExecuted', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Method3SetUpOKandPasses', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(LExecControl);
  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed,
    'Test should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
//  Check(Pos('Setup Failed: ', LTest.ErrorMessage) = 1,
//    'ErrorMessage should contain <Setup Failed: > but was ' +
//    LTest.ErrorMessage);

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed,
    'Test should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  Check(LExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(LExecControl.ExecutionCount));
end;

procedure TTestTestCaseHandlesNonMethodFailures.CheckTeardownFailure;
var
  LTest: ITest;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(THasTeardownFailure.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'ExecutedMethodPasses', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'ExecutedMethodTearDownFails', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  FTestProject.Run(LExecControl);
  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed,
    'Test should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  Check(LExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(LExecControl.ExecutionCount));
//  Check(Pos('TearDown Failed: ', LTest.ErrorMessage) = 1,
//    'ErrorMessage should contain <TearDown Failed: > but was ' +
//    LTest.ErrorMessage);
end;

{ TTestExceptInSetupAndTearDown }

function TTestExceptInSetupAndTearDown.SetUpRan: boolean;
begin
  Result := FSetUpRan;
end;

function TTestExceptInSetupAndTearDown.MethodRan: boolean;
begin
  Result := FMethodRan;
end;

function TTestExceptInSetupAndTearDown.TearDownRan: boolean;
begin
  Result := FTearDownRan;
end;

procedure TTestExceptInSetupAndTearDown.SetUpOnce;
begin
  FSetUpRan := False;
  FMethodRan := False;
  FTearDownRan := False;
end;

procedure TTestExceptInSetupAndTearDown.SetUp;
begin
  FSetUpRan := True;
  raise EAbort.Create('Deliberate exception in SetUp');
end;

procedure TTestExceptInSetupAndTearDown.TestWithSetUpAndTearDownFailure;
begin
  FMethodRan := True;
  Check(SetUpRan, 'SetUp should have run');
end;

procedure TTestExceptInSetupAndTearDown.TearDown;
begin
  FTearDownRan := True;
  raise EAbort.Create('Deliberate exception in TearDown');
end;

{ TTestPeripheryExceptions }

procedure TTestPeripheryExceptions.ExecStatusCallBack(const ATest: ITest);
var
  LStatusRecord: TStatusRecord;
begin
  if (ATest <> nil) and Supports(ATest, ITest) then
  begin
    SetLength(FStatusArray, Length(FStatusArray)+1);
    LStatusRecord.ExecStatus := (ATest as ITest).ExecStatus;
    LStatusRecord.AName := (ATest as ITest).DisplayedName;
    FStatusArray[Length(FStatusArray)-1] := LStatusRecord;
  end;
end;

procedure TTestPeripheryExceptions.ExecStatusCallBackMsg(const ATest: ITest);
begin
  if ATest.ErrorMessage <> '' then
    FErrorMsg := ATest.ErrorMessage;
end;

procedure TTestPeripheryExceptions.SetUp;
begin
  FErrorMsg := '';
  FAnExecControl := nil;
  FTestProject := nil;
  SetLength(FStatusArray,0);
  FList := TInterfaceList.Create;
end;

procedure TTestPeripheryExceptions.TearDown;
begin
  FErrorMsg := '';
  FTest := nil;
  FList := nil;
  FAnExecControl := nil;
  SetLength(FStatusArray,0);
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestPeripheryExceptions.TestITestExceptionInDoSetUp;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestExceptInDoSetup.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestExceptInDoSetUp', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBackMsg;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators

  LTest := FTestProject.FindFirstTest;//EnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.DisplayedName = 'TestExceptInDoSetUp', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Test should not have executed');
  Check(FAnExecControl.ExecutionCount = 0,
    'Test Run count should be 0 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  CheckEqualsString('SetUpOnce failed: Deliberate exception in SetUpOnce', FErrorMsg, 'SetUpOnce error message is incorrect');
end;

procedure TTestPeripheryExceptions.TestITestExceptionInSetUp;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestExceptInSetup.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Test1stMethodWithSetUpFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Test2ndMethodWithSetUpFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBackMsg;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  Check(FAnExecControl.ExecutionCount = 0,
    'Test Run count should be 0 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  Check(FAnExecControl.ExecutionCount = 0,
    'Test Run count should be 0 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  CheckEqualsString('SetUp failed: Deliberate exception in SetUp',
    FErrorMsg, 'SetUp error message is incorrect');
end;

procedure TTestPeripheryExceptions.TestITestExceptionInSetUpAndTearDown;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestExceptInSetupAndTearDown.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestWithSetUpAndTearDownFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Should not find more methods');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBackMsg;
  FTestProject.Run(FAnExecControl);
  FTestProject.FindFirstTest; //This resets all iterators

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');

  CheckTrue((LTest as IWhatRan).SetUpRan, 'Setup should have run');
  CheckFalse((LTest as IWhatRan).MethodRan, 'Method should not have run');
  CheckFalse((LTest as IWhatRan).TearDownRan, 'TearDown should not have run');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  Check(FAnExecControl.ExecutionCount = 0,
    'Test run count should be 0 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  CheckEqualsString('SetUp failed: Deliberate exception in SetUp',
    FErrorMsg, 'No other aspect should have executed');
end;

procedure TTestPeripheryExceptions.TestITestExceptionInSetUpReports;
var
  LCount: Integer;
  LExeName: string;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TTestExceptInSetup.Suite);
  FTestProject := TestFramework.Projects;
  Check(Assigned(FTestProject), 'Test Project should exist');
  LExecControl := TestFramework.Projects.ExecutionControl;
  LExecControl.ExecStatusUpdater := ExecStatusCallBack;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  FTestProject.Run(LExecControl);
  LCount := Length(FStatusArray);
  Check(LCount > 0, 'StatusListner not called');
  Check(LCount = 8, 'Wrong Count, should be 8 but was ' + IntToStr(LCount));
  // The following is the mimimalist reporting scenario.
  // Only changes that need reporting generate a callback.

  LExeName := ExtractFileName(ParamStr(0));
  Check(FStatusArray[ 0].AName = LExeName,                        FStatusArray[ 0].AName);
  Check(FStatusArray[ 1].AName = 'TTestExceptInSetup',            FStatusArray[ 1].AName);
  Check(FStatusArray[ 2].AName = 'Test1stMethodWithSetUpFailure', FStatusArray[ 2].AName);
  Check(FStatusArray[ 3].AName = 'Test1stMethodWithSetUpFailure', FStatusArray[ 3].AName);
  Check(FStatusArray[ 4].AName = 'Test2ndMethodWithSetUpFailure', FStatusArray[ 3].AName);
  Check(FStatusArray[ 5].AName = 'Test2ndMethodWithSetUpFailure', FStatusArray[ 3].AName);
  Check(FStatusArray[ 6].AName = 'TTestExceptInSetup',            FStatusArray[ 4].AName);
  Check(FStatusArray[ 7].AName = LExeName,                        FStatusArray[ 5].AName);

  Check(FStatusArray[ 0].ExecStatus = _Running,  LExeName + ' not notified of Running state');
  Check(FStatusArray[ 1].ExecStatus = _Running, 'TTestExceptInSetup not notified of Running state');
  Check(FStatusArray[ 2].ExecStatus = _Running, 'Test1stMethodWithSetUpFailure not notified of Error state');
  Check(FStatusArray[ 3].ExecStatus = _Error,   'Test1stMethodWithSetUpFailure not notified of Error state');
  Check(FStatusArray[ 4].ExecStatus = _Running, 'Test2ndMethodWithSetUpFailure not notified of Error state');
  Check(FStatusArray[ 5].ExecStatus = _Error,   'Test2ndMethodWithSetUpFailure not notified of Error state');
  Check(FStatusArray[ 6].ExecStatus = _Error,   'TTestExceptInSetup not notified of _Error state');
  Check(FStatusArray[ 7].ExecStatus = _Error,    LExeName + ' not notified of _Error state');
end;

procedure TTestPeripheryExceptions.TestITestExceptionInTearDown;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestExceptInTearDown.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Test1stMethodWithTearDownFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Test2ndMethodWithTearDownFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.ExecStatusUpdater := ExecStatusCallBackMsg;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));
  Check(LTest.ExecStatus = _Error, 'TestCase should have been Error');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test procedure should have been Error');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test procedure should have been Error');
  Check(FAnExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  CheckEqualsString('TearDown failed: Deliberate exception in TearDown',
    FErrorMsg, 'Error message incorrect');
end;

procedure TTestPeripheryExceptions.TestITestExceptionInDoTearDown;
var
  LTest: ITest;
  LCount: Integer;
  LTestStatus: TExecutionStatus;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestExceptInDoTearDown.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'DoTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  LTestStatus := FTestProject.Run(FAnExecControl);
  Check(LTestStatus = _Error, 'TestCase should have been Error');

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'TestCase should have been Error');

  LTest := FTestProject.FindNextTest;//EnabledProc;
  Check(LTest.DisplayedName = 'DoTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed, 'Test should have been Passed');
  Check(FAnExecControl.ExecutionCount = 1,
    'Test Run count should be 1 but was ' + IntToStr(FAnExecControl.ExecutionCount));
end;

{ TTestSimplePass1 }

procedure TTestSimplePass1.SetUp;
begin
  FMethodRan := False;
end;

function TTestSimplePass1.MethodRan: boolean;
begin
  Result := FMethodRan;
end;

procedure TTestSimplePass1.SimpleMethod1;
begin
  FMethodRan := True;
  Check(True);
end;

procedure TTestSimplePass1.SimpleMethodFails;
begin
  FMethodRan := True;
  Check(False, 'Deliberate failure to demo effect');
end;

{ TTestSimplePass2 }

procedure TTestSimplePass2.SetUp;
begin
  FMethodRan := False;
end;

function TTestSimplePass2.MethodRan: boolean;
begin
  Result := FMethodRan;
end;

procedure TTestSimplePass2.SimpleMethod2;
begin
  FMethodRan := True;
  Check(True);
end;

{ TTestTearDownFailInTestSuite }

procedure TTestTearDownFailInTestSuite.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestTearDownFailInTestSuite.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestTearDownFailInTestSuite.TestTheWholeSuiteRuns;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('ASuite', [TTestSimplePass1.Suite,
                                          TTestExceptInDoTearDown.Suite,
                                          TTestSimplePass2.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'ASuite', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSimplePass1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethod1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethodFails', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'DoTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(not Assigned(LTest), 'LTest should now be nil');


  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);
  Check(FAnExecControl.ExecutionCount = 4,
    'Test Run count should be 4 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.FailureCount = 1,
    'Test Failure count should be 1 but was ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.ErrorCount = 1,
    'Test Error count should be 1 but was ' + IntToStr(FAnExecControl.ErrorCount));

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));
  Check(LTest.DisplayedName = 'ASuite', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error,
    'ASuite Status should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSimplePass1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Failed,
    'TTestSimplePass1 Status should have been _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));
  Check((LTest as IMethodRan).MethodRan, 'SimpleMethod1 did not run');

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'SimpleMethod1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed,
    'SimpleMethod1 Status should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'SimpleMethodFails', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Failed,
    'SimpleMethodFails Status should have been _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error,
    'THasTeardownFailure Status should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  Check(Pos('TearDownOnce failed: ', LTest.ErrorMessage) = 1,
    'ErrorMessage should contain <TearDownOnce failed: > but was ' +
    LTest.ErrorMessage);


  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'DoTestExceptInDoTearDown', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed,
    'ExecutedMethodTearDownFails Status should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed,
    'TTestSimplePass2 Status should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));
  Check((LTest as IMethodRan).MethodRan, 'SimpleMethod2 did not run');

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed,
    'SimpleMethod2 Status should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextTest;
  Check(not Assigned(LTest), 'LTest should now be nil');
//  Check(False, 'Lets me know we got this far');
end;

{ TTestGens2Errors }

procedure TTestGens2Errors.GenEAbortError1;
begin
  raise EAbort.Create('Deliberate Abort Error');
end;

procedure TTestGens2Errors.GenExceptError2;
begin
  raise Exception.Create('Deliberate Exception');
end;

{ TTestErrorReporting }

procedure TTestErrorReporting.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestErrorReporting.TearDown;
begin
  FAnExecControl := nil;
  FTestProject.TestSetUpData := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestErrorReporting.TestErrorsReport;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestGens2Errors.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenEAbortError1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenExceptError2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Error, 'Test should have been Error');
  Check(FAnExecControl.ErrorCount = 2,
    'ErrorCount should be 2 but is ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.FailureCount = 0,
    'FailCount should be 0 but is' + IntToStr(FAnExecControl.FailureCount));
  Check(FAnExecControl.WarningCount = 0,
    'WarningCount should be 0 but is' + IntToStr(FAnExecControl.WarningCount));
  Check(FAnExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestGens2Failures }

procedure TTestGens2Failures.GenFailError1;
begin
  Fail('First Failure');
end;

procedure TTestGens2Failures.GenStopError2;
begin
  StopTests('Testing Stop');
end;

procedure TTestErrorReporting.TestFailuresReport;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestGens2Failures.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenFailError1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenStopError2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Test should have been Fail');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Stopped, 'Test should have been Stopped');
  Check(FAnExecControl.ErrorCount = 0,
    'ErrorCount should be 0 but is ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.FailureCount = 2,
    'FailCount should be 2 but is' + IntToStr(FAnExecControl.FailureCount));
  Check(FAnExecControl.WarningCount = 0,
    'WarningCount should be 0 but is' + IntToStr(FAnExecControl.WarningCount));
//  Check(LTest.ErrorMessage = 'Testing Stopped: Testing Stop',
//    'Wrong Message for warning = ' + LTest.ErrorMessage);
  Check(FAnExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestGens2Warnings }

procedure TTestGens2Warnings.GenCheckFail;
begin
  // Force Fail on no checks executed
  FailsOnNoChecksExecuted := FailsOnNoChecksExecuted;
end;

procedure TTestGens2Warnings.GenCheckWarning1;
begin
  FailsOnNoChecksExecuted := False; // Override GUI setting
end;

procedure TTestGens2Warnings.GenCheckWarning2;
begin
  FailsOnNoChecksExecuted := False; // Override GUI setting
end;

{$IFNDEF CLR}
{ TTestGens2MemWarning2 }

procedure TTestGens2MemWarning2.TeardownOnce;
begin
  FreeAndNil(FObj1);
  FreeAndNil(FObj2);
end;

procedure TTestGens2MemWarning2.GenMemWarning1;
begin
  FObj1 := TObject.Create;
  FailsOnMemoryLeak := False;
  CheckFalse(FailsOnMemLeakDetection, 'Should reflect state of FailsOnMemoryLeak');
  Check(Assigned(FObj1), 'Object must exist to be deliberately leaked');
end;

procedure TTestGens2MemWarning2.GenMemWarning2;
begin
  FObj2 := TObject.Create;
  FailsOnMemoryLeak := False;
  Check(Assigned(FObj2), 'Object must exist to be deliberately leaked');
end;
{$ENDIF}

procedure TTestErrorReporting.TestCheckWarningsReport;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestGens2Warnings.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenCheckFail', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenCheckWarning1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.FailsOnNoChecksExecuted := True;
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Failed, 'Should have been _Failure');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Warning, 'Test should have been _Warning');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Warning, 'Test should have warned when check overridden');
  Check(FAnExecControl.ErrorCount = 0,
    'ErrorCount should be 0 but is ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.FailureCount = 1,
    'FailCount should be 1 but is' + IntToStr(FAnExecControl.FailureCount));
  Check(FAnExecControl.WarningCount = 2,
    'WarningCount should be 2 but is' + IntToStr(FAnExecControl.WarningCount));
  Check(FAnExecControl.ExecutionCount = 3,
    'Test Run count should be 3 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{$IFNDEF CLR}
procedure TTestErrorReporting.TestMemWarningsReport;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestGens2MemWarning2.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenMemWarning1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'GenMemWarning2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.FailsOnMemoryLeak := True;
  CheckTrue(FAnExecControl.FailsOnMemLeakDetection, 'Should reflect state of FailsOnMemoryLeak');
  FAnExecControl.InhibitStackTrace := True;
  FTestProject.Run(FAnExecControl);

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus =
  {$IFDEF FASTMM}
    _Warning , '1st Test should have been _Warning');
  {$ELSE}
    _Passed , '1st Test should have been _Passed when FastmMM not deployed');
  {$ENDIF}
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus =
  {$IFDEF FASTMM}
    _Warning , '2nd Test should have been _Warning');
  {$ELSE}
    _Passed , '2nd Test should have been _Passed when FastmMM not deployed');
  {$ENDIF}
  Check(FAnExecControl.ErrorCount = 0,
    'ErrorCount should be 0 but is ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.FailureCount = 0,
    'FailCount should be 0 but is' + IntToStr(FAnExecControl.FailureCount));
  {$IFDEF FASTMM}
  Check(FAnExecControl.WarningCount = 2,
    'WarningCount should be 2 but is ' + IntToStr(FAnExecControl.WarningCount));
  {$ELSE}
  Check(FAnExecControl.WarningCount = 0,
    'WarningCount should be 0 but is ' + IntToStr(FAnExecControl.WarningCount));
  {$ENDIF}
  Check(LTest.ErrorMessage = '',
    'Wrong Message for warning = ' + LTest.ErrorMessage);
  Check(FAnExecControl.ExecutionCount = 2,
    'Test Run count should be 2 but was ' + IntToStr(FAnExecControl.ExecutionCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;
{$ENDIF}

{ TTestWith3Procs }

procedure TTestWith3Procs.Proc1;
begin
  Check(True, 'Deliberate pass');
end;

procedure TTestWith3Procs.Proc2;
begin
  Check(False, 'Deliberate Fail');
end;

procedure TTestWith3Procs.Proc3;
begin
  Check(True, 'Deliberate pass');
end;

{ TTestExcludedTestReporting }

function TTestExcludedTestReporting.IsTestExcluded(const ATest: ITest): Boolean;
begin
  Assert(Assigned(ATest), 'Test not assigned in  TTestExcludedTestReporting');
  Result := not ATest.Excluded;
end;

procedure TTestExcludedTestReporting.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestExcludedTestReporting.TearDown;
begin
  FAnExecControl := nil;
  FTestProject.TestSetUpData := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestExcludedTestReporting.VerifyExcludedTestGetReported;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TTestWith3Procs.Suite]);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Proc1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
  LTest.Excluded := True; //Now hide from execution.

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Proc2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
  LTest.Excluded := True; //Now hide from execution.

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'Proc3', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;
  FAnExecControl.IndividuallyEnabledTest := IsTestExcluded;
  FTestProject.Run(FAnExecControl);

  FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Ready, 'Test should not have executed');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Ready, 'Test should not have executed');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest.ExecStatus = _Passed, 'Test should have executed');
  Check(FAnExecControl.ErrorCount = 0,
    'ErrorCount should be 0 but is ' + IntToStr(FAnExecControl.ErrorCount));
  Check(FAnExecControl.FailureCount = 0,
    'FailCount should be 2 but is' + IntToStr(FAnExecControl.FailureCount));
  Check(FAnExecControl.WarningCount = 0,
    'WarningCount should be 0 but is' + IntToStr(FAnExecControl.WarningCount));
  Check(FAnExecControl.ExecutionCount= 1,
    'Test Run count should be 1 but was ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.ExcludedCount = 2,
    'ExcludedCount should be 2 but was ' + IntToStr(FAnExecControl.ExcludedCount));
  LTest := FTestProject.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Test should be nil');
end;

{ TTestCreateNamedTest }

procedure TTestCreateNamedTest.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestCreateNamedTest.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestCreateNamedTest.TestCanEnable1stOfMultipleMethods;
var
  LATest: TTestCase3;
  LCount: Integer;
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  LATest := TTestCase3.Create('TestProcedure1');
  LCount := LATest.Count;
  Check(LCount = 1, 'Should only be 1 enabled test but there is ' + IntToStr(LCount));
  Check(LATest.DisplayedName  = 'TTestCase3');

  TestFramework.RegisterTest(LATest);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure1', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
end;

procedure TTestCreateNamedTest.TestCanEnable2ndOfMultipleMethods;
var
  LATest: TTestCase3;
  LCount: Integer;
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  LATest := TTestCase3.Create('TestProcedure2');
  LCount := LATest.Count;
  Check(LCount = 1, 'Should only be 1 enabled test but there is ' + IntToStr(LCount));
  Check(LATest.DisplayedName  = 'TTestCase3');

  TestFramework.RegisterTest(LATest);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestProcedure2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
end;

procedure TTestCreateNamedTest.TestCanExecuteNamedMethodSetup;
var
  LATest: TTestFullTestCaseCalled;
  LCount: Integer;
  LReturnStatus: TExecutionStatus;
  LTest: ITest;
  LExecCtrl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  LATest := TTestFullTestCaseCalled.Create('TestExecutesNamedMethodSetup');
  TestFramework.RegisterTest(LATest);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');

  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' +
    IntToStr(LCount));
  Check((LATest as ITestCase) <> nil, 'NextTest returned wrong class');
  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TestExecutesNamedMethodSetup', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should now be be _Ready');
  LExecCtrl := FTestProject.ExecutionControl;
  LExecCtrl.InhibitStackTrace := True; // This disarms the StackTrace
  try
    LReturnStatus := (LTest as ITestMethod).Run(LATest, 'TestExecutesNamedMethodSetup', LExecCtrl);
    Check(LReturnStatus = _Failed, 'Returned value should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

    LCount := FTestProject.Count;
    Check(LCount = 1, 'Count <> 1 Count = ' + IntToStr(LCount));
    LReturnStatus := FTestProject.Run(LExecCtrl);
    Check(LReturnStatus = _Passed, 'Returned value should be _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  finally
    LExecCtrl := nil;
  end;
end;

{ TTestChecks }

procedure TTestChecks.RunOldTestCheck;
{$IFNDEF CLR}
var
  Ls1, Ls2, Ls3 :WideString;
{$ENDIF}
begin  // I make it 27 calls to check for win32 code here
  Check(true, 'Check');
  CheckEquals(1, 1,                   'CheckEquals    Integer');
  CheckNotEquals(1, 2,                'CheckNotEquals Integer');
  CheckEquals(1.0, 1.1, 0.15,         'CheckEquals    Double');
  CheckNotEquals(1.0, 1.16, 0.15,     'CheckNotEquals Double');
  CheckEqualsString('abc', 'abc',     'CheckEquals    String');
  CheckNotEqualsString('abc', 'abcd', 'CheckNotEquals String');
  CheckEquals(true, true,             'CheckEquals    Boolean');
  CheckNotEquals(true, false,         'CheckNotEquals Boolean');

  CheckEqualsBin(1, 1,                'CheckEqualsBin  Longword');
  CheckNotEqualsBin(1, 2,             'CheckNotEqualsBin  Longword');
  CheckEqualsHex(1, 1,                'CheckEqualsHex  Longword');
  CheckNotEqualsHex(1, 2,             'CheckNotEqualsHex  Longword');

  CheckNull(TObject(nil),        'CheckNull');
  CheckNotNull(TObject(self),    'CheckNotNull object');
  CheckSame(TObject(self), self, 'CheckSame    object');

  // need the TTestCase(self) cast to work around Delphi typing quirks
  CheckNull(TestFramework.TTestCase(nil) as ITest,        'CheckNull');
  {$IFDEF CLR}
  CheckNotNull(self as ITest,    'CheckNotNull interface');
  CheckSame(self as ITest, self as ITest, 'CheckSame    interface');
  {$ELSE}
    {$IFNDEF VER130}
      {$IFNDEF VER140}
        {$WARN UNSAFE_CAST OFF}
      {$ENDIF}
    {$ENDIF}
  CheckNotNull(TestFramework.TTestCase(self) as ITest,    'CheckNotNull interface');
  CheckSame(TestFramework.TTestCase(self) as ITest, TestFramework.TTestCase(self) as ITest, 'CheckSame    interface');
    {$IFNDEF VER130}
      {$IFNDEF VER140}
        {$WARN UNSAFE_CAST ON}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  CheckIs(self, TObject, 'self not a TObject');

  {$IFNDEF CLR}
  Ls1 := 'aaa'#1024;
  Ls2 := 'aaa';
  Ls3 := 'bbb';

  CheckEqualsWideString(Ls1, Ls1, 'CheckEquals WideString');
  CheckNotEqualsWideString(Ls1, Ls2, 'CheckNotEquals WideString');
  CheckNotEqualsWideString(Ls2, Ls3, 'CheckNotEquals WideString');
  {$ENDIF}

  CheckTrue(true, 'CheckTrue');
  CheckFalse(false, 'CheckFalse');
end;

procedure TTestChecks.TestCheckP;
begin
  Check(True);
end;

procedure TTestChecks.TestCheckF;
begin
  Check(False);
end;

procedure TTestChecks.TestCheckMsgP;
begin
  Check(True, 'AMsg');
end;

procedure TTestChecks.TestCheckMsgF;
begin
  Check(False, 'AMsg');
end;

procedure TTestChecks.TestCheckTrueP;
begin
  CheckTrue(True);
end;

procedure TTestChecks.TestCheckTrueF;
begin
  CheckTrue(False);
end;

procedure TTestChecks.TestCheckTrueMsgP;
begin
  CheckTrue(True, 'AMsg');
end;

procedure TTestChecks.TestCheckTrueMsgF;
begin
  CheckTrue(False, 'AMsg');
end;

procedure TTestChecks.TestCheckFalseP;
begin
  CheckFalse(False);
end;

procedure TTestChecks.TestCheckFalseF;
begin
  CheckFalse(True);
end;

procedure TTestChecks.TestCheckFalseMsgP;
begin
  CheckFalse(False, 'AMsg');
end;

procedure TTestChecks.TestCheckFalseMsgF;
begin
  CheckFalse(True, 'AMsg');
end;

{$IFNDEF CLR}
  {$IFNDEF UNICODE}
    {$IFNDEF VER130}
      {$IFNDEF VER140}
        {$WARN UNSAFE_CODE OFF}
      {$ENDIF}
    {$ENDIF}
procedure TTestChecks.TestCheckEqualsMemP;
const
  AStr: string = 'ABCD';
begin
  CheckEqualsMem(@AStr, @AStr, 4);
end;

procedure TTestChecks.TestCheckEqualsMemF;
const
  AStr: string = 'ABCD';
  Diff: string = 'ABCD';
begin
  CheckEqualsMem(@AStr, @Diff, 4);
end;

procedure TTestChecks.TestCheckNotEqualsMemP;
const
  AStr: string = 'ABCD';
  Diff: string = 'ABCD';
begin
  CheckNotEqualsMem(@AStr, @Diff, 4);
end;

procedure TTestChecks.TestCheckNotEqualsMemF;
const
  AStr: string = 'ABCD';
  Diff: string = 'ABCD';
begin
  CheckNotEqualsMem(@AStr, @Diff, 4);
end;

procedure TTestChecks.TestCheckEqualsMemMsgP;
const
  AStr: string = 'ABCD';
begin
  CheckEqualsMem(@AStr, @AStr, 4, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsMemMsgF;
const
  AStr: string = 'ABCD';
  Diff: string = 'ABCD';
begin
  CheckEqualsMem(@AStr, @Diff, 4, 'AMsg');
end;

  {$IFNDEF VER130}
    {$IFNDEF VER140}
      {$WARN UNSAFE_CAST OFF}
    {$ENDIF}
  {$ENDIF}

procedure TTestChecks.TestCheckNotEqualsMemMsgP;
const
  AStr: string = 'ABCD';
  Diff: string = 'ABCD';
begin
  CheckNotEqualsMem(@AStr, @Diff, 4, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsMemMsgF;
const
  AStr: string = 'ABCD';
begin
  CheckNotEqualsMem(@AStr, @AStr, 4, 'AMsg');
end;
    {$IFNDEF VER130}
      {$IFNDEF VER140}
        {$WARN UNSAFE_CODE ON}
        {$WARN UNSAFE_CAST ON}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF UNICODE}
{$ENDIF CLR}


procedure TTestChecks.TestCheckEqualsIntP;
begin
  CheckEquals(Integer(1), Integer(1));
end;

procedure TTestChecks.TestCheckEqualsIntF;
begin
  CheckEquals(Integer(1), Integer(2));
end;

procedure TTestChecks.TestCheckEqualsIntMsgP;
begin
  CheckEquals(Integer(1), Integer(1), 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsIntMsgF;
begin
  CheckEquals(Integer(1), Integer(2), 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsIntP;
begin
  CheckEquals(Integer(1), Integer(2));
end;

procedure TTestChecks.TestCheckNotEqualsIntF;
begin
  CheckEquals(Integer(1), Integer(1));
end;

procedure TTestChecks.TestCheckNotEqualsIntMsgP;
begin
  CheckEquals(Integer(1), Integer(2), 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsIntMsgF;
begin
  CheckEquals(Integer(1), Integer(1), 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsInt64P;
begin
  CheckEquals(Int64(1), Int64(1));
end;

procedure TTestChecks.TestCheckEqualsInt64F;
begin
  CheckEquals(Int64(1), Int64(2));
end;

procedure TTestChecks.TestCheckEqualsInt64MsgF;
begin
  CheckEquals(Int64(1), Int64(2), 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsInt64P;
begin
  CheckEquals(Int64(1), Int64(2));
end;

procedure TTestChecks.TestCheckNotEqualsInt64F;
begin
  CheckEquals(Int64(1), Int64(1));
end;

procedure TTestChecks.TestCheckNotEqualsInt64MsgP;
begin
  CheckEquals(Int64(1), Int64(2), 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsInt64MsgF;
begin
  CheckEquals(Int64(1), Int64(1), 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsInt64MsgP;
begin
  CheckEquals(Int64(1), Int64(1), 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsExtndP;
begin
  CheckEquals(1, 1);
  CheckEquals(1.0, 1.0);
end;

procedure TTestChecks.TestCheckNotEqualsExtndP;
begin
  CheckNotEquals(5.0, 5.0);
end;

procedure TTestChecks.TestCheckNotEqualsExtndF;
begin
  CheckNotEquals(1.1, 2.2);
end;

procedure TTestChecks.TestCheckEqualsExtndF;
begin
  CheckEquals(1.1, 2.2);
end;

procedure TTestChecks.TestCheckEqualsExtndMsgP;
begin
  CheckEquals(2.0, 2.0, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsExtndMsgF;
begin
  CheckEquals(2.0, 3.0, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsExtndMsgP;
begin
  CheckNotEquals(5.0, 5.0, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsExtndMsgF;
begin
  CheckNotEquals(1.1, 2.2, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsExtndDeltaP;
begin
  CheckEquals(3.0, 3.0, 1);
end;

procedure TTestChecks.TestCheckEqualsExtndDeltaF;
begin
  CheckEquals(1.0, 1.1, 0.05);
end;

procedure TTestChecks.TestCheckNotEqualsExtndDeltaP;
begin
  CheckEquals(1.0, 1.1, 0.05);
end;

procedure TTestChecks.TestCheckNotEqualsExtndDeltaF;
begin
  CheckEquals(3.0, 3.0, 1);
end;

procedure TTestChecks.TestCheckEqualsExtndDeltaMsgP;
begin
  CheckEquals(4.0, 4.0, 2, 'Full deal');
end;

procedure TTestChecks.TestCheckEqualsExtndDeltaMsgF;
begin
  CheckEquals(1.0, 1.1, 0.05, 'Fails');
end;

procedure TTestChecks.TestCheckNotEqualsExtndDeltaMsgP;
begin
  CheckEquals(1.0, 1.1, 0.05, 'Fails');
end;

procedure TTestChecks.TestCheckNotEqualsExtndDeltaMsgF;
begin
  CheckEquals(4.0, 4.0, 2, 'Full deal');
end;

procedure TTestChecks.TestCheckEqualsStrP;
begin
  CheckEquals('xyZ', 'xyZ');
end;

procedure TTestChecks.TestCheckEqualsStrF;
begin
  CheckEquals('XYz', 'xyZ');
end;

procedure TTestChecks.TestCheckNotEqualsStrP;
begin
  CheckEquals('XYz', 'xyZ');
end;

procedure TTestChecks.TestCheckNotEqualsStrF;
begin
  CheckEquals('xyZ', 'xyZ');
end;

procedure TTestChecks.TestCheckEqualsStrMsgP;
begin
  CheckEquals('xyZ', 'xyZ', 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsStrMsgF;
begin
  CheckEquals('XYz', 'xyZ', 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsStringP;
begin
  CheckEqualsString('abc', 'abc');
end;

procedure TTestChecks.TestCheckEqualsStringF;
begin
  CheckEqualsString('abc', 'xyz');
end;

procedure TTestChecks.TestCheckNotEqualsStringP;
begin
  CheckNotEquals('abc', '123');
end;

procedure TTestChecks.TestCheckNotEqualsStringF;
begin
  CheckNotEquals('abc', 'abc');
end;

procedure TTestChecks.TestCheckEqualsStringMsgP;
begin
  CheckEquals('xyZ', 'xyZ', 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsStringMsgF;
begin
  CheckEquals('XYz', 'xyZ', 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsStringMsgP;
begin
  CheckNotEquals('abc', '123', 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsStringMsgF;
begin
  CheckEqualsString('abc', 'def', 'AMsg');
end;

destructor TTestChecks.Destroy;
begin
  FreeAndNil(Obj1);
  FreeAndNil(Obj2);
  inherited;
end;

procedure TTestChecks.DontRaiseException;
begin
  Check(True);
end;

procedure TTestChecks.RaiseAgreedException;
begin
  raise EDivByZero.Create('Forced EDivByZero');
end;

procedure TTestChecks.RaiseDifferentException;
begin
  raise ERangeError.Create('Forced ERangeError');
end;

procedure TTestChecks.SetUp;
begin
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.InhibitStackTrace := True;
  s1 := '';
  s2 := '';
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  if not Assigned(Obj2) then
    Obj2 := TObject.Create;
end;

procedure TTestChecks.TearDown;
begin
  FAnExecControl := nil;
  s1 := '';
  s2 := '';
  FreeAndNil(Obj1);
  FreeAndNil(Obj2);
end;

procedure TTestChecks.TestBooleanCheckEqualsP;
begin
  CheckEquals(true, true);
end;

procedure TTestChecks.TestBooleanCheckEqualsF;
begin
  CheckEquals(true, false);
end;

procedure TTestChecks.TestBooleanCheckEqualsMsgP;
begin
  CheckEquals(true, true, 'AMsg');
end;

procedure TTestChecks.TestBooleanCheckEqualsMsgF;
begin
  CheckEquals(true, false, 'AMsg');
end;

procedure TTestChecks.TestBooleanNotCheckEqualsP;
begin
  CheckNotEquals(true, false);
end;

procedure TTestChecks.TestBooleanNotCheckEqualsF;
begin
  CheckNotEquals(true, true);
end;

procedure TTestChecks.TestBooleanNotCheckEqualsMsgF;
begin
  CheckNotEquals(true, false, 'AMsg');
end;

procedure TTestChecks.TestBooleanNotCheckEqualsMsgP;
begin
  CheckNotEquals(true, true, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsBinP;
begin
  CheckEqualsBin(1,1);
end;

procedure TTestChecks.TestCheckEqualsBinF;
begin
  CheckEqualsBin(1,0);
end;

procedure TTestChecks.TestCheckNotEqualsBinP;
begin
  CheckNotEqualsBin(1,0);
end;

procedure TTestChecks.TestCheckNotEqualsBinF;
begin
  CheckNotEqualsBin(1,1);
end;

procedure TTestChecks.TestCheckEqualsHexP;
begin
  CheckEqualsHex(1,1);
end;

procedure TTestChecks.TestCheckEqualsHexF;
begin
  CheckEqualsHex(1,2);
end;

procedure TTestChecks.TestCheckNotEqualsHexP;
begin
  CheckNotEqualsHex(1,2);
end;

procedure TTestChecks.TestCheckNotEqualsHexF;
begin
  CheckNotEqualsHex(1,1);
end;

procedure TTestChecks.TestCheckNullP;
begin
  CheckNull(TObject(nil));
end;

procedure TTestChecks.TestCheckNullF;
begin
  CheckNull(TObject(self));
end;

procedure TTestChecks.TestCheckNotNullP;
begin
  CheckNotNull(TObject(self));
end;

procedure TTestChecks.TestCheckNotNullF;
begin
  CheckNotNull(TObject(nil));
end;

procedure TTestChecks.TestCheckExceptionF;
var
  LAnExceptionClass: TClass;
begin
  LAnExceptionClass := EDivByZero;
  {$IFDEF CLR}
    CheckException('RaiseDifferentException', LAnExceptionClass);
  {$ELSE}
    CheckException(RaiseDifferentException, LAnExceptionClass);
  {$ENDIF}
end;

procedure TTestChecks.TestCheckExceptionN;
var
  LAnExceptionClass: TClass;
begin
  LAnExceptionClass := nil;
  {$IFDEF CLR}
    CheckException('DontRaiseException', LAnExceptionClass);
  {$ELSE}
    CheckException(DontRaiseException, LAnExceptionClass);
  {$ENDIF}
end;

procedure TTestChecks.TestCheckExceptionP;
var
  LAnExceptionClass: TClass;
begin
  LAnExceptionClass := EDivByZero;
  {$IFDEF CLR}
    CheckException('RaiseAgreedException', LAnExceptionClass);
  {$ELSE}
    CheckException(RaiseAgreedException, LAnExceptionClass);
  {$ENDIF}
end;

{$IFNDEF VER130}
  {$IFNDEF VER140}
    {$WARN UNSAFE_CAST OFF}
  {$ENDIF}
{$ENDIF}
procedure TTestChecks.TestCheckSameIfaceP;
begin
  {$IFDEF CLR}
    CheckSame((self as ITest), (self as ITest));
  {$ELSE}
    CheckSame(TestFramework.TTestCase(self) as ITest, TestFramework.TTestCase(self) as ITest);
  {$ENDIF}
end;

procedure TTestChecks.TestCheckSameIfaceF;
begin
  CheckSame((TestFramework.TestProject as ITestProject), (self as ITest));
end;

procedure TTestChecks.TestCheckSameIfaceMsgP;
begin
  {$IFDEF CLR}
    CheckSame((self as ITest), (self as ITest), 'AMsg');
  {$ELSE}
    CheckSame(TestFramework.TTestCase(self) as ITest, TestFramework.TTestCase(self) as ITest, 'AMsg');
  {$ENDIF}
end;

procedure TTestChecks.TestCheckSameIfaceMsgF;
begin
  CheckSame((TestFramework.TestProject as ITestProject), (self as ITest), 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameIfaceP;
begin
  CheckNotSame((TestFramework.TestProject as ITestProject), (self as ITest));
end;

procedure TTestChecks.TestCheckNotSameIfaceF;
begin
  {$IFDEF CLR}
    CheckNotSame((self as ITest), (self as ITest));
  {$ELSE}
    CheckNotSame(TestFramework.TTestCase(self) as ITest, TestFramework.TTestCase(self) as ITest);
  {$ENDIF}
end;

procedure TTestChecks.TestCheckNotSameIfaceMsgP;
begin
  CheckNotSame((TestFramework.TestProject as ITestProject), (self as ITest), 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameIfaceMsgF;
begin
  {$IFDEF CLR}
    CheckNotSame((self as ITest), (self as ITest), 'AMsg');
  {$ELSE}
    CheckNotSame(TestFramework.TTestCase(self) as ITest, TestFramework.TTestCase(self) as ITest, 'AMsg');
  {$ENDIF}
end;

{$IFNDEF VER130}
  {$IFNDEF VER140}
    {$WARN UNSAFE_CAST ON}
  {$ENDIF}
{$ENDIF}
procedure TTestChecks.TestCheckSameObjP;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckSame(Obj1, Obj1);
end;

procedure TTestChecks.TestCheckSameObjF;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  if not Assigned(Obj2) then
    Obj2 := TObject.Create;
  CheckSame(Obj1, Obj2);
end;

procedure TTestChecks.TestCheckSameObjMsgP;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckSame(Obj1, Obj1, 'AMsg');
end;

procedure TTestChecks.TestCheckSameObjMsgF;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  if not Assigned(Obj2) then
    Obj2 := TObject.Create;
  CheckSame(Obj1, Obj2, 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameObjP;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  if not Assigned(Obj2) then
    Obj2 := TObject.Create;
  CheckNotSame(Obj1, Obj2);
end;

procedure TTestChecks.TestCheckNotSameObjF;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckNotSame(Obj1, Obj1);
end;

procedure TTestChecks.TestCheckNotSameObjMsgP;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  if not Assigned(Obj2) then
    Obj2 := TObject.Create;
  CheckNotSame(Obj1, Obj2, 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameObjMsgF;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckNotSame(Obj1, Obj1, 'AMsg');
end;

procedure TTestChecks.TestCheckSameClassP;
begin
  CheckEquals(TObjectA, TObjectA);
end;

procedure TTestChecks.TestCheckSameClassF;
begin
  CheckEquals(TObjectA, TObjectB);
end;

procedure TTestChecks.TestCheckSameClassMsgP;
begin
  CheckEquals(TObjectA, TObjectA, 'AMsg');
end;

procedure TTestChecks.TestCheckSameClassMsgF;
begin
  CheckEquals(TObjectA, TObjectB, 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameClassP;
begin
  CheckNotEquals(TObjectA, TObjectB);
end;

procedure TTestChecks.TestCheckNotSameClassF;
begin
  CheckNotEquals(TObjectA, TObjectA);
end;

procedure TTestChecks.TestCheckNotSameClassMsgP;
begin
  CheckNotEquals(TObjectA, TObjectB, 'AMsg');
end;

procedure TTestChecks.TestCheckNotSameClassMsgF;
begin
  CheckNotEquals(TObjectA, TObjectA, 'AMsg');
end;

procedure TTestChecks.TestCheckInheritsP;
begin
  CheckInherits(TTypeA, TTypeAAofANotB);
end;

procedure TTestChecks.TestCheckInheritsF;
begin
  CheckInherits(TTypeB, TTypeAAofANotB);
end;

procedure TTestChecks.TestCheckIsP;
begin
  CheckIs(self, TObject);
end;

procedure TTestChecks.TestCheckIsF;
begin
  CheckIs(self, TTypeB);
end;

{$IFNDEF CLR}
  {$IFNDEF UNICODE}
procedure TTestChecks.TestCheckEqualsWSP;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckEquals(s1,s2);
end;

procedure TTestChecks.TestCheckEqualsWSF;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckEquals(s1, s2);
end;

procedure TTestChecks.TestCheckNotEqualsWSP;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckNotEquals(s1, s2);
end;

procedure TTestChecks.TestCheckNotEqualsWSF;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckNotEquals(s1,s2);
end;

procedure TTestChecks.TestCheckEqualsWSMsgP;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckEquals(s1,s2, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsWSMsgF;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckEquals(s1, s2, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsWSMsgF;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckNotEquals(s1,s2, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsWSMsgP;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckNotEquals(s1, s2, 'AMsg');
end;
  {$ENDIF UNICODE}
procedure TTestChecks.TestCheckEqualsWideStringP;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckEqualsWideString(s1, s2);
end;

procedure TTestChecks.TestCheckEqualsWideStringF;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckEqualsWideString(s1, s2);
end;

procedure TTestChecks.TestCheckEqualsWideStringMsgP;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckEqualsWideString(s1, s2, 'AMsg');
end;

procedure TTestChecks.TestCheckEqualsWideStringMsgF;
begin
  s1 := 'aaa'#1024;
  s2 := '123';
  CheckEqualsWideString(s1, s2, 'AMsg');
end;

procedure TTestChecks.TestCheckNotEqualsWideStringP;
begin
  s1 := 'aaa'#1024;
  s2 := 'aaa';
  CheckNotEqualsWideString(s1, s2);
end;

procedure TTestChecks.TestCheckNotEqualsWideStringF;
begin
  s1 := 'aaa';
  s2 := 'aaa';
  CheckNotEqualsWideString(s1, s2);
end;
{$ENDIF CLR}

{ TTestInheritsFrom }

procedure TTestCheckInherits.TearDown;
begin
  FreeandNil(Obj1);
end;

destructor TTestCheckInherits.Destroy;
begin
  FreeandNil(Obj1);
  inherited;
end;

procedure TTestCheckInherits.ExpectedIsNil;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckInherits(nil, Obj1.ClassType);
end;

procedure TTestCheckInherits.ActualIsNil;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckInherits(Obj1.ClassType, nil);
end;

{ TTestCheckIs }
destructor TTestCheckIs.Destroy;
begin
  FreeandNil(Obj1);
  inherited;
end;

procedure TTestCheckIs.TearDown;
begin
  FreeandNil(Obj1);
end;

procedure TTestCheckIs.ExpectedIsNil;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckIs(nil, Obj1.ClassType);
end;

procedure TTestCheckIs.ActualIsNil;
begin
  if not Assigned(Obj1) then
    Obj1 := TObject.Create;
  CheckIs(Obj1, nil);
end;

{ TTestCheckTests }

function TTestCheckTests.CheckPassOrFail(const TestName: string;
                                         const Outcome: TExecutionStatus): boolean;
var
  LTest: ITest;
  LTestCase: ITestCase;
  LExecControl: ITestExecControl;
  LOutcome: TExecutionStatus;
begin
  try
    Result := False;
    Check(TestName <> '', 'TestName was empty');
    LTestCase := TTestChecks.Create(TestName);

    LTestCase.Reset;
    repeat
      LTest := LTestCase.FindNextEnabledProc;
    until ((LTest = nil) or (LTest.MethodsName = TestName));
    Check(Assigned(LTest), TestName +
      ' is not a published procedure of TTestChecks');

    LExecControl := TestFramework.TestExecControl;
    LExecControl.InhibitStackTrace := True;
    LOutcome := (LTest as ITestMethod).Run(LTestCase, TestName, LExecControl);
    Result := LOutcome = Outcome;
    Check(Result, LTest.DisplayedName + ' should return ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(Outcome)));
  finally
    LExecControl.CurrentTest := nil;
    LExecControl := nil;
    LTestCase := nil;
    LTest := nil;
  end;
end;

// This function cycles through each test method in a TestCase and halts
// if the test result <> Outcome and returns a message from the offending test.
// It is specifically designed to allow all error handling tests be verified
function TTestCheckTests.CheckAllTestsPassOrFail(const TestCaseType: TTestCaseType;
                                                 const Outcome: TExecutionStatus;
                                                 out   ErrorMsg: string): boolean;
var
  LTest: ITest;
  LTestCase: ITestCase;
  LExecControl: ITestExecControl;
  LTestResult: TExecutionStatus;
begin
  Result := False;
  LExecControl := TestFramework.TestExecControl;
  LExecControl.InhibitStackTrace := True;
  LTestCase := TestCaseType.Create;
  try
    repeat
      LTest := LTestCase.FindNextEnabledProc;
      if Assigned(LTest) then
      begin
        LTestResult := (LTest as ITestMethod).Run(LTestCase, LTest.DisplayedName, LExecControl);
        Result := LTestResult = Outcome;
        if not Result then
          ErrorMsg := LTest.ParentPath + '.' + LTest.DisplayedName +
            ': ' + GetEnumName(TypeInfo(TExecutionStatus), Ord(LTestResult)) + ': ';
      end;
    until (LTest=nil) or not Result;
  finally
    LExecControl.CurrentTest := nil;
    LExecControl := nil;
    LTestCase := nil;
    LTest := nil;
  end;
end;

procedure TTestCheckTests.TestAllCheckCalls;
var
  LErrorMsg:      string;
begin
  Check(CheckPassOrFail('TestCheckP', _Passed), 'TestCheckP failed');
  Check(CheckPassOrFail('TestCheckF', _Failed), 'TestCheckF failed');
  Check(CheckPassOrFail('TestCheckMsgP', _Passed), 'TestCheckMsgP failed');
  Check(CheckPassOrFail('TestCheckMsgF', _Failed), 'TestCheckMsgF failed');
  Check(CheckPassOrFail('TestCheckTrueP', _Passed), 'TestCheckTrueP failed');
  Check(CheckPassOrFail('TestCheckTrueF', _Failed), 'TestCheckTrueF failed');
  Check(CheckPassOrFail('TestCheckFalseP', _Passed), 'TestCheckFalseP failed');
  Check(CheckPassOrFail('TestCheckFalseF', _Failed), 'TestCheckFalseF failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsP', _Passed), 'TestBooleanCheckEqualsP failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsF', _Failed), 'TestBooleanCheckEqualsF failed');
  Check(CheckPassOrFail('TestCheckEqualsIntP', _Passed), 'CheckEqualsIntP failed');
  Check(CheckPassOrFail('TestCheckEqualsIntF', _Failed), 'CheckEqualsIntF failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsMsgP', _Passed), 'TestBooleanCheckEqualsMsgP failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsMsgF', _Failed), 'TestBooleanCheckEqualsMsgF failed');
  Check(CheckPassOrFail('TestCheckEqualsIntMsgP', _Passed), 'CheckEqualsIntMsgP failed');
  Check(CheckPassOrFail('TestCheckEqualsIntMsgF', _Failed), 'CheckEqualsIntMsgF failed');
  Check(CheckPassOrFail('TestCheckEqualsInt64P', _Passed), 'CheckEqualsInt64P failed');
  Check(CheckPassOrFail('TestCheckEqualsInt64F', _Failed), 'CheckEqualsInt64F failed');
  Check(CheckPassOrFail('TestCheckEqualsInt64MsgP', _Passed), 'CheckEqualsInt64MsgP failed');
  Check(CheckPassOrFail('TestCheckEqualsInt64MsgF', _Failed), 'CheckEqualsInt64MsgF failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndP', _Passed), 'CheckEqualsExtndP failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndF', _Failed), 'CheckEqualsExtndF failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndMsgP', _Passed), 'CheckEqualsExtndMsgP failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndMsgF', _Failed), 'CheckEqualsExtndMsgF failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndDeltaP', _Passed), 'CheckEqualsExtndDeltaP failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndDeltaF', _Failed), 'CheckEqualsExtndDeltaF failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndDeltaMsgP', _Passed), 'CheckEqualsExtndDeltaMsgP failed');
  Check(CheckPassOrFail('TestCheckEqualsExtndDeltaMsgF', _Failed), 'CheckEqualsExtndDeltsMsgF failed');
  Check(CheckPassOrFail('TestCheckEqualsStrP', _Passed), 'CheckEqualsStr failed');
  Check(CheckPassOrFail('TestCheckEqualsStrF', _Failed), 'CheckEqualsStr failed');
  Check(CheckPassOrFail('TestCheckEqualsStrMsgP', _Passed), 'CheckEqualsStrMsg failed');
  Check(CheckPassOrFail('TestCheckEqualsStrMsgF', _Failed), 'CheckEqualsStrMsg failed');
  Check(CheckPassOrFail('TestCheckEqualsStringP', _Passed), 'CheckEqualsString failed');
  Check(CheckPassOrFail('TestCheckEqualsStringF', _Failed), 'CheckEqualsString failed');
  Check(CheckPassOrFail('TestCheckNotEqualsStringP', _Passed), 'CheckNotEqualsString failed');
  Check(CheckPassOrFail('TestCheckNotEqualsStringF', _Failed), 'CheckNotEqualsString failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsP', _Passed), 'TestBooleanCheckEquals failed');
  Check(CheckPassOrFail('TestBooleanCheckEqualsF', _Failed), 'TestBooleanCheckEquals failed');
  Check(CheckPassOrFail('TestBooleanNotCheckEqualsP', _Passed), 'TestBooleanNotCheckEqualsP failed');
  Check(CheckPassOrFail('TestBooleanNotCheckEqualsF', _Failed), 'TestBooleanNotCheckEqualsF failed');
  Check(CheckPassOrFail('TestCheckEqualsBinP', _Passed), 'TestCheckEqualsBinP failed');
  Check(CheckPassOrFail('TestCheckEqualsBinF', _Failed), 'TestCheckEqualsBinF failed');
  Check(CheckPassOrFail('TestCheckNotEqualsBinP', _Passed), 'TestCheckNotEqualsBinP failed');
  Check(CheckPassOrFail('TestCheckNotEqualsBinF', _Failed), 'TestCheckNotEqualsBinF failed');
  Check(CheckPassOrFail('TestCheckEqualsHexP', _Passed), 'TestCheckEqualsHexP failed');
  Check(CheckPassOrFail('TestCheckEqualsHexF', _Failed), 'TestCheckEqualsHexF failed');
  Check(CheckPassOrFail('TestCheckNotEqualsHexP', _Passed), 'TestCheckNotEqualsHexP failed');
  Check(CheckPassOrFail('TestCheckNotEqualsHexF', _Failed), 'TestCheckNotEqualsHexF failed');
  Check(CheckPassOrFail('TestCheckNullP', _Passed), 'TestCheckNullP failed');
  Check(CheckPassOrFail('TestCheckNullF', _Failed), 'TestCheckNullF failed');
  Check(CheckPassOrFail('TestCheckNotNullP', _Passed), 'TestCheckNotNullP failed');
  Check(CheckPassOrFail('TestCheckNotNullF', _Failed), 'TestCheckNotNullF failed');
  Check(CheckPassOrFail('TestCheckExceptionP', _Passed), 'TestCheckExceptionP failed');
  Check(CheckPassOrFail('TestCheckExceptionF', _Failed), 'TestCheckExceptionF failed');
  Check(CheckPassOrFail('TestCheckExceptionN', _Passed), 'TestCheckExceptionN failed');
  Check(CheckPassOrFail('TestCheckSameIfaceP', _Passed), 'TestCheckSameIfaceP failed');
  Check(CheckPassOrFail('TestCheckSameIfaceF', _Failed), 'TestCheckSameIfaceF failed');
  Check(CheckPassOrFail('TestCheckSameIfaceMsgP', _Passed), 'TestCheckSameIfaceMsgP failed');
  Check(CheckPassOrFail('TestCheckSameIfaceMsgF', _Failed), 'TestCheckSameIfaceMsgF failed');
  Check(CheckPassOrFail('TestCheckNotSameIfaceP', _Passed), 'TestCheckNotSameIfaceP failed');
  Check(CheckPassOrFail('TestCheckNotSameIfaceF', _Failed), 'TestCheckNotSameIfaceF failed');
  Check(CheckPassOrFail('TestCheckNotSameIfaceMsgP', _Passed), 'TestCheckNotSameIfaceMsgP failed');
  Check(CheckPassOrFail('TestCheckNotSameIfaceMsgF', _Failed), 'TestCheckNotSameIfaceMsgF failed');
  Check(CheckPassOrFail('TestCheckSameObjP', _Passed), 'TestCheckSameObjP failed');
  Check(CheckPassOrFail('TestCheckSameObjF', _Failed), 'TestCheckSameObjF failed');
  Check(CheckPassOrFail('TestCheckSameObjMsgP', _Passed), 'TestCheckSameObjMsgP failed');
  Check(CheckPassOrFail('TestCheckSameObjMsgF', _Failed), 'TestCheckSameObjMsgF failed');
  Check(CheckPassOrFail('TestCheckNotSameObjP', _Passed), 'TestCheckNotSameObjP failed');
  Check(CheckPassOrFail('TestCheckNotSameObjF', _Failed), 'TestCheckNotSameObjF failed');
  Check(CheckPassOrFail('TestCheckNotSameObjMsgP', _Passed), 'TestCheckNotSameObjMsgP failed');
  Check(CheckPassOrFail('TestCheckNotSameObjMsgF', _Failed), 'TestCheckNotSameObjMsgF failed');
  Check(CheckPassOrFail('TestCheckSameClassP', _Passed), 'TestCheckSameClassP failed');
  Check(CheckPassOrFail('TestCheckSameClassF', _Failed), 'TestCheckSameObjF failed');
  Check(CheckPassOrFail('TestCheckSameClassMsgP', _Passed), 'TestCheckSameObjMsgP failed');
  Check(CheckPassOrFail('TestCheckSameClassMsgF', _Failed), 'TestCheckSameObjMsgF failed');
  Check(CheckPassOrFail('TestCheckNotSameClassP', _Passed), 'TestCheckNotSameObjP failed');
  Check(CheckPassOrFail('TestCheckNotSameClassF', _Failed), 'TestCheckNotSameObjF failed');
  Check(CheckPassOrFail('TestCheckNotSameClassMsgP', _Passed), 'TestCheckNotSameObjMsgP failed');
  Check(CheckPassOrFail('TestCheckNotSameClassMsgF', _Failed), 'TestCheckNotSameObjMsgF failed');
  Check(CheckPassOrFail('TestCheckInheritsP', _Passed), 'TestCheckInheritsP failed');
  Check(CheckPassOrFail('TestCheckInheritsF', _Failed), 'TestCheckInheritsF failed');
  Check(CheckPassOrFail('TestCheckIsP', _Passed), 'TestCheckIsP failed');
  {$IFNDEF CLR}
    {$IFNDEF UNICODE}
    Check(CheckPassOrFail('TestCheckEqualsWSP', _Passed), 'CheckEqualsWSP failed');
    Check(CheckPassOrFail('TestCheckEqualsWSF', _Failed), 'CheckEqualsWSP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWSP', _Passed), 'CheckEqualsWSP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWSF', _Failed), 'CheckEqualsWSP failed');
    Check(CheckPassOrFail('TestCheckEqualsWSMsgP', _Passed), 'CheckEqualsWSMsgP failed');
    Check(CheckPassOrFail('TestCheckEqualsWSMsgF', _Failed), 'CheckEqualsWSMsgP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWSMsgP', _Passed), 'CheckEqualsWSMsgP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWSMsgF', _Failed), 'CheckEqualsWSMsgP failed');
    Check(CheckPassOrFail('TestCheckEqualsMemMsgP', _Passed), 'CheckEqualsMemMsgP failed');
    Check(CheckPassOrFail('TestCheckEqualsMemMsgF', _Failed), 'CheckEqualsMemMsgF failed');
    Check(CheckPassOrFail('TestCheckNotEqualsMemMsgP', _Passed), 'CheckEqualsMemMsgP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsMemMsgF', _Failed), 'CheckEqualsMemMsgF failed');
    {$ENDIF UNICODE}
    Check(CheckPassOrFail('TestCheckEqualsWideStringP', _Passed), 'CheckEqualsWideStringP failed');
    Check(CheckPassOrFail('TestCheckEqualsWideStringF', _Failed), 'CheckEqualsWideStringF failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWideStringP', _Passed), 'CheckNotEqualsWideStringP failed');
    Check(CheckPassOrFail('TestCheckNotEqualsWideStringF', _Failed), 'CheckNotEqualsWideStringF failed');
  {$ENDIF}

  Check(CheckAllTestsPassOrFail(TTestCheckInherits, _Failed, LErrorMsg), LErrorMsg);
  Check(CheckAllTestsPassOrFail(TTestCheckIs, _Failed, LErrorMsg), LErrorMsg);
end;

{ TTestChecksCount }

procedure TTestChecksCount.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
end;

procedure TTestChecksCount.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestChecksCount.TestAllChecksCount;
const
  Win32AndCLRCommonTests    = 110;
  Win32AndCLRCommonFailures = 54;
  Win32OnlyTests = 22;  // Comprises 22 win32 only checks of which 16 are not UNICODE either
  TotalWin32ChecksCallable = Win32AndCLRCommonTests + Win32OnlyTests;
  NonUNICODETests = 16;
  NonUNICODEFailures = 8;
  Win32OnlyFailures = 10;
var
  LStatus : TExecutionStatus;
  LTests: Cardinal;
  LFailures: Integer;
  LChecksCalled: Integer;
begin
  {$IFDEF CLR}
    LTests        := Win32AndCLRCommonTests;
    LFailures     := Win32AndCLRCommonFailures ;
    LChecksCalled := Win32AndCLRCommonTests + 22; // 22 calls inside RunOldTestCheck
  {$ELSE}
    {$IFDEF UNICODE}
      LTests        := Win32AndCLRCommonTests + Win32OnlyTests - NonUNICODETests;
      LFailures     := Win32AndCLRCommonFailures + Win32OnlyFailures - NonUNICODEFailures + 1; //Why the 1??
      LChecksCalled := Win32AndCLRCommonTests + 25 + Win32OnlyTests - NonUNICODETests;
    {$ELSE}
      LTests        := Win32AndCLRCommonTests + Win32OnlyTests;
      LFailures     := Win32AndCLRCommonFailures + Win32OnlyFailures;
      LChecksCalled := Win32AndCLRCommonTests + 25 + Win32OnlyTests;
    {$ENDIF UNICODE}
  {$ENDIF CLR}

  TestFramework.RegisterTest(TTestChecks.Suite);
  FTestProject := TestFramework.TestProject;
  Check(Assigned(FTestProject), 'FTestProjet not assigned');
  FAnExecControl := FTestProject.ExecutionControl;
  Check(Assigned(FAnExecControl), 'FAnExecControl not assigned');
  FAnExecControl.InhibitStackTrace := True;
  LStatus := FTestProject.Run(FAnExecControl);
  Check(LStatus = _Failed,
    'Status should be _Failed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LStatus)));
  Check(Assigned(FAnExecControl), 'FTestProjet was cleared and is not assigned');

  Check(FAnExecControl.ExecutionCount = LTests,
    'Execution count should be ' + IntToStr(LTests) + ' but is ' +
      IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.CheckCalledCount = LChecksCalled,
    'CheckCalledCount count should be ' + IntToStr(LChecksCalled) + ' but is ' +
      IntToStr(FAnExecControl.CheckCalledCount));
  Check(FAnExecControl.FailureCount = LFailures,
    'FailureCount count should be ' + IntToStr(LFailures) + ' but is ' +
      IntToStr(FAnExecControl.FailureCount));

  Check(FAnExecControl.WarningCount = 0,
    'WarningCount count should be 0 but is ' + IntToStr(FAnExecControl.WarningCount));
  Check(FAnExecControl.ErrorCount = 0,
    'ErrorCount count should be 0 but is ' + IntToStr(FAnExecControl.ErrorCount));
end;

{ TElapsedTimesTests }

procedure TElapsedTimesTests.Quick;
begin
  Check(Self.ElapsedTime = 0,
    'Time should be zero but was ' + ElapsedDHMS(Self.ElapsedTime));
end;

procedure TElapsedTimesTests.Sleep20;
begin
  Check(Self.ElapsedTime = 0,
    'Time should be zero but was ' + ElapsedDHMS(Self.ElapsedTime));
  Sleep(20);
end;

procedure TElapsedTimesTests.Sleep40;
begin
  Check(Self.ElapsedTime = 0,
    'Time should be zero but was ' + ElapsedDHMS(Self.ElapsedTime));
  Sleep(40);
end;

{ TTestsElapsedTimes }

procedure TTestsElapsedTimes.SetUp;
begin
  FAnExecControl := TestFramework.TestExecControl;
  gTimer.Clear;
  gTimer.Start;
  Sleep(1); // Attempt to sync up to thread slice
end;

procedure TTestsElapsedTimes.TearDown;
begin
  gTimer.Stop;
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestsElapsedTimes.TestElapsedTestCase;
var
  LReturnStatus: TExecutionStatus;
  LTest: ITest;
  LHold: Extended;
  LExecControl: ITestExecControl;
begin
// A +- 3 ms tolerance has been built in to cater for rounding errors
  TestFramework.RegisterTest(TElapsedTimesTests.Suite);
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Passed,
    'TElapsedTimesTests failed with returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'TElapsedTimesTests = nil');
  Check(LTest.DisplayedName = 'TElapsedTimesTests',
    'Name should be TElapsedTimesTests but was ' +
      LTest.DisplayedName);
  LHold := LTest.ElapsedTime;

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Quick = nil');
  Check(LTest.ElapsedTime <= 3,
    'Elapsed time for Quick execution too slow, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Sleep20 = nil');
  Check(LTest.ElapsedTime >= 17,
    'Elapsed time for Sleep(20) execution too fast, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  Check(LTest.ElapsedTime <= 23,
    'Elapsed time for Sleep(20) execution too slow, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Sleep40 = nil');
  Check(LTest.ElapsedTime >= 37,
    'Elapsed time for Sleep(40) execution too fast, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  Check(LTest.ElapsedTime <= 43,
    'Elapsed time for Sleep(40) execution too slow, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  Check(LHold >= 55, // Held over so individual times get tested first.
    'Elapsed time for TElapsedTimesTests execution too fast, was ' +
      ElapsedDHMS(LTest.ElapsedTime));

  Check(LHold <= 65, // Held over so individual times get tested first.
    'Elapsed time for TElapsedTimesTests execution too slow, was ' +
      ElapsedDHMS(LTest.ElapsedTime));
end;

procedure TTestsElapsedTimes.TestElapsedSuite;
var
  LReturnStatus: TExecutionStatus;
  LTest: ITest;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTests('OneSleepyTestCase', [TElapsedTimesTests.Suite]);
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Passed,
    'TElapsedTimesTests failed with returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'OneSleepyTestCase = nil');
  Check(LTest.DisplayedName = 'OneSleepyTestCase',
    'Name should be OneSleepyTestCase but was ' +
      LTest.DisplayedName);
  Check(LTest.ElapsedTime >= 27,
    'Elapsed time for TElapsedTimesTests execution too fast, was ' +
      ElapsedDHMS(LTest.ElapsedTime));
end;

procedure TTestsElapsedTimes.TestElapsedSuites;
var
  LReturnStatus: TExecutionStatus;
  LTest: ITest;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTests('TwoSleepyTestCases',
    [TElapsedTimesTests.Suite, TElapsedTimesTests.Suite]);
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Passed,
    'TElapsedTimesTests failed with returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'TwoSleepyTestCases = nil');
  Check(LTest.DisplayedName = 'TwoSleepyTestCases',
    'Name should be TwoSleepyTestCases but was ' +
      LTest.DisplayedName);
  Check(LTest.ElapsedTime >= 57,
    'Elapsed time for TElapsedTimesTests execution too fast, was ' +
      ElapsedDHMS(LTest.ElapsedTime));
end;

procedure TTestsElapsedTimes.TestElapsedProject;
var
  LReturnStatus: TExecutionStatus;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTests('TwoSleepyTestCases',
    [TElapsedTimesTests.Suite, TElapsedTimesTests.Suite]);
  TestFramework.RegisterTests('TwoMoreSleepyTestCases',
    [TElapsedTimesTests.Suite, TElapsedTimesTests.Suite]);

  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Passed,
    'TElapsedTimesTests failed with returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  Check(FTestProject.ElapsedTime >= 117,
    'Elapsed time for TElapsedTimesTests execution too fast, was ' +
      ElapsedDHMS(FTestProject.ElapsedTime));
end;


{ TSetupData }

function TSetupData.get_SetupData: string;
begin
  Result := FSetupData;
end;

procedure TSetupData.set_SetupData(const Value: string);
begin
  FSetupData := Value;
end;

{ TSetupDataAccessTest }

procedure TSetupDataAccessTest.CheckSetupData;
begin
  Check(Assigned(TestSetUpData), 'TestSetUpData = nil');
  Check(Supports(TestSetUpData, ICheckTestSetUpData), 'ICheckTestSetUpData not suported');
  Check((TestSetUpData as ICheckTestSetUpData).SetupData  = 'ABCDEF',
    'TestSetUpData should be ABCDEF but is ' +
      (TestSetUpData as ICheckTestSetUpData).SetupData);

  (TestSetUpData as ICheckTestSetUpData).SetupData := 'JKLMNO';
  Check((TestSetUpData as ICheckTestSetUpData).SetupData  = 'JKLMNO',
    'TestSetUpData should be JKLMNO but is ' +
      (TestSetUpData as ICheckTestSetUpData).SetupData);
end;

{ TTestSetUpDataAccess }

procedure TTestSetUpDataAccess.SetUp;
begin
  FTestProject := nil;
  FAnExecControl := nil; //TestFramework.TestExecControl;
end;

procedure TTestSetUpDataAccess.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;


procedure TTestSetUpDataAccess.CheckSetUpDataPassedDown;
var
  LReturnStatus: TExecutionStatus;
  LSetupData: ICheckTestSetUpData;
begin
  TestFramework.RegisterTest(TSetupDataAccessTest.Create);
  FTestProject := TestFramework.TestProject;
  Check(Assigned(FTestProject), 'FTestProject = nil');
  FAnExecControl := FTestProject.ExecutionControl;
  FAnExecControl.InhibitStackTrace := True;

  LSetupData := nil;
  FAnExecControl.TestSetUpData := LSetupData;
  LReturnStatus := FTestProject.Run(FAnExecControl);
  Check(LReturnStatus = _Failed,
    'TSetupDataAccessTest should be _Failed but returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  LSetupData := TSetupData.Create;
  FAnExecControl.TestSetUpData := LSetupData;
  FTestProject.ExecStatus := _Ready;
  LReturnStatus := FTestProject.Run(FAnExecControl);
  Check(LReturnStatus = _Failed,
    'TSetupDataAccessTest should be _Failed but returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  LSetupData.SetupData := 'ABCDEF';
  FAnExecControl.TestSetUpData := LSetupData;
  FTestProject.ExecStatus := _Ready;
  LReturnStatus := FTestProject.Run(FAnExecControl);
  Check((LReturnStatus = _Passed) or (LReturnStatus = _Warning),
    'TSetupDataAccessTest should be _Passed or _Warning but returned status of ' +
    GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));

  Check(LSetupData.SetupData = 'JKLMNO', 'SetupData should be JKLMNO but is ' +
    LSetupData.SetupData);
end;


{ TTestMemoryMonitor }

{$IFDEF FASTMM}
procedure TLeakTestClass.ClearVars;
begin
  if Assigned(AnObject) then
  try
    FreeAndNil(AnObject);
  except
    AnObject := nil;
  end;
end;

procedure TLeakTestClass.Setup;
begin
  FailsOnMemoryLeak := True;
end;

procedure TLeakTestClass.TearDownOnce;
begin
  ClearVars;
end;

{ TNoLeaks }

procedure TNoLeaks.Setup;
begin
  AnObject := TObject.Create;
end;

procedure TNoLeaks.RunATest;
begin
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

procedure TNoLeaks.TearDown;
begin
  ClearVars;
end;

{ TLeaksInSetup } // By assigning in setup and never releasing

procedure TLeaksInSetup.Setup;
begin
  AnObject := TObject.Create;
end;

procedure TLeaksInSetup.RunATest;
begin
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

{ TLeaksInRun }

procedure TLeaksInRun.RunATest;
begin
  AnObject := TObject.Create;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

{ TLeaksInTearDown }

procedure TLeaksInTearDown.RunATest;
begin
  Check(not Assigned(AnObject), 'AnObject should not be assigned here');
end;

procedure TLeaksInTearDown.TearDown;
begin
  AnObject := TObject.Create;
end;

procedure TTestDefaultIsWarnButIgnoreLeaks.RunATest;
begin
  FailsOnMemoryLeak := False;
  AnObject := TObject.Create;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

procedure TTestContainsAllowedTObjectLeak.RunATest;
begin
  FailsOnMemoryLeak := True;
  CheckTrue(FailsOnMemLeakDetection, 'Should reflect state of FailsOnMemoryLeak');
  AllowedMemoryLeakSize := 16;
  AnObject := TObject.Create;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

procedure TTestContainsAllowedTObjectLeakByList.RunATest;
begin
  FailsOnMemoryLeak := True;
  SetAllowedLeakArray([1, 7, 16]);
  AnObject := TObject.Create;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

procedure TTestContainsAllowedTObjectLeakByEmptyList.RunATest;
begin
  FailsOnMemoryLeak := True;
  AllowedMemoryLeakSize := 16;
  SetAllowedLeakArray([]);
  AnObject := TObject.Create;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;

procedure TTestContainsAllowedLeakArrayLongList.RunATest;
begin
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
  SetAllowedLeakArray([1, 7, 16, 55]);  // causes deliberate failure
end;


procedure TTestCaseIgnoresLeaksAtSetUpLevel.SetUp;
begin
  AnObject := TObject.Create;
end;

procedure TTestCaseIgnoresLeaksAtSetUpLevel.RunATest;
begin
  FailsOnMemoryLeak := True;
  IgnoresMemoryLeakInSetUpTearDown := True;
  Check(Assigned(AnObject), 'AnObject should still be assigned here');
end;


procedure TTestCaseIgnoresLeaksAtTearDownLevel.TearDown;
begin
  AnObject := TObject.Create;
end;

procedure TTestCaseIgnoresLeaksAtTearDownLevel.RunATest;
begin
  FailsOnMemoryLeak := True;
  IgnoresMemoryLeakInSetUpTearDown := True;
  CheckFalse(Assigned(AnObject), 'AnObject should not be assigned here');
end;

{------------------}
procedure TTestMemoryMonitor.TestAlloweLeakListIterator;
var  // Test based on current allowed array length of 4
  ListIteratorValue: TListIterator;
  LValue: Integer;
begin
  AllowedMemoryLeakSize := 44;
  // Note. Using more values than necessary to show it's handled gracefully
  SetAllowedLeakArray([3, 97, -10]);
  LValue := AllowedMemoryLeakSize;
  Check(LValue = 44, 'Should return 44 but returned ' +
    IntToStr(LValue));

  ListIteratorValue := AllowedLeaksIterator;
  LValue := ListIteratorValue;
  Check(LValue = 44, 'ListIterator should return 44 but returned ' +
    IntToStr(LValue));
  LValue := ListIteratorValue;
  Check(LValue =  3, 'ListIterator should return 3 but returned ' +
    IntToStr(LValue));
  LValue := ListIteratorValue;
  Check(LValue = 97, 'ListIterator should return 97 but returned ' +
    IntToStr(LValue));
  LValue := ListIteratorValue;
  Check(LValue = -10, 'ListIterator should return -10 but returned ' +
    IntToStr(LValue));
  LValue := ListIteratorValue;
  Check(LValue =  0, 'ListIterator should return 0 but returned ' +
    IntToStr(LValue));
end;

const
  ForceFailOnMemoryLeak: boolean = True;
  NoFailureOnMemoryLeak : boolean = False;

procedure TTestMemoryMonitor.TestDefaultIsIgnoreLeaks;
begin
  Check(CheckExecutionResult(TTestDefaultIsWarnButIgnoreLeaks, 'Run',
    NoFailureOnMemoryLeak, _Warning), 'TTestDefaultIsIgnoreLeaks failed');
end;

procedure TTestMemoryMonitor.TestNoLeaks;
begin
  Check(CheckExecutionResult(TNoLeaks, 'Run',
    ForceFailOnMemoryLeak, _Passed), 'TNoLeaks failed');
end;

procedure TTestMemoryMonitor.TestRunLeaks;
begin
  Check(CheckExecutionResult(TLeaksInRun, 'Run',
    ForceFailOnMemoryLeak, _Failed), 'TLeaksInSetup failed');
end;

procedure TTestMemoryMonitor.TestSetupLeaks;
begin
  Check(CheckExecutionResult(TLeaksInSetup, 'Run',
    ForceFailOnMemoryLeak, _Failed), 'TLeaksInRun failed');
end;

procedure TTestMemoryMonitor.TestTearDownLeaks;
begin
  Check(CheckExecutionResult(TLeaksInTearDown, 'Run',
    ForceFailOnMemoryLeak, _Failed), 'TLeaksInTearDown failed');
end;

procedure TTestMemoryMonitor.TestContainsAllowedTObjectLeak;
begin
  Check(CheckExecutionResult(TTestContainsAllowedTObjectLeak, 'Run',
    ForceFailOnMemoryLeak, _Warning), 'TTestContainsAllowedTObjectLeakfailed');
end;

procedure TTestMemoryMonitor.TestContainsAllowedTObjectLeakByList;
begin
  Check(CheckExecutionResult(TTestContainsAllowedTObjectLeakByList, 'Run',
    ForceFailOnMemoryLeak, _Warning), 'TTestContainsAllowedTObjectLeakByList');
end;

procedure TTestMemoryMonitor.TestContainsAllowedTObjectLeakByEmptyList;
begin
  Check(CheckExecutionResult(TTestContainsAllowedTObjectLeakByEmptyList, 'Run',
    ForceFailOnMemoryLeak, _Warning), 'TTestContainsAllowedTObjectLeakByEmptyList');
end;

procedure TTestMemoryMonitor.TestContainsAllowedLeakArrayLongList;
begin
  Check(CheckExecutionResult(TTestContainsAllowedLeakArrayLongList, 'Run',
    ForceFailOnMemoryLeak, _Failed), 'TTestContainsAllowedLeakArrayLongList');
end;

procedure TTestMemoryMonitor.TestCaseIgnoresLeaksAtSetupTearDownLevel;
begin
  Check(CheckExecutionResult(TTestCaseIgnoresLeaksAtSetupLevel, 'Run',
    ForceFailOnMemoryLeak, _Warning), 'TTestCaseIgnoresLeaksAtSetupLevel');
  Check(CheckExecutionResult(TTestCaseIgnoresLeaksAtTearDownLevel, 'Run',
    ForceFailOnMemoryLeak, _Warning), 'TTestCaseIgnoresLeaksAtTearDownLevel');
end;

{ TPassOrFail }

function TPassOrFail.CheckExecutionResult(const ATestClass: TheTestClass;
                                          const TestName: string;
                                          const CanDetectLeak: boolean;
                                          const Outcome: TExecutionStatus): boolean;
var
  LTest: ITest;
  LStatus: TExecutionStatus;
begin
  Result := False;
  FAnExecControl := nil;
  try
    Check(TestName <> '', 'TestName was empty');
    TestFramework.RegisterTest(ATestClass.Suite);
    FTestProject := TestFramework.TestProject;
    FAnExecControl := FTestProject.ExecutionControl;
    LTest := FTestProject.FindNextEnabledProc;
    Check(Assigned(LTest), TestName +
      ' is not a published procedure of TTestChecks');
    FAnExecControl.FailsOnMemoryLeak := CanDetectLeak;
    FAnExecControl.InhibitStackTrace := True;
    LStatus := (FTestProject as ITestSuite).Run(FAnExecControl);
    Check(LStatus = Outcome,
      LTest.DisplayedName + ' should return ' +
        GetEnumName(TypeInfo(TExecutionStatus), Ord(Outcome)) +
        ' Error message was ' + LTest.ErrorMessage);
    Result := not Result;
  finally
    FAnExecControl := nil;
    RemoveProjectManager;
  end;
end;

procedure TPassOrFail.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := TestFramework.TestProject;
end;

procedure TPassOrFail.TearDown;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

{ TLeaksAndPasses }

procedure TLeaksAndPasses.SetUpOnce;
begin
  FreeAndNil(FSetUpObject);
  FreeAndNil(FRunObject);
  FreeAndNil(FTearDownObject);
  FreeAndNil(FRunObjectA);
  FreeAndNil(FRunObjectB);
  FSequenceNo := 0;
end;

procedure TLeaksAndPasses.SetUp;
begin
  Inc(FSequenceNo);
  if FSequenceNo = 2 then
    FSetUpObject := TObject.Create;
end;

procedure TLeaksAndPasses.TearDown;
begin
  if FSequenceNo = 5 then
    FTearDownObject := TObject.Create;
end;

procedure TLeaksAndPasses.TearDownOnce;
begin
  SetUpOnce;
end;

procedure TLeaksAndPasses.ThisPassesBeforeSetUpLeak;
begin
  Check(FSequenceNo = 1,
    'SequenceNo should be 1 in ThisPassesBeforeSetUpLeak but was ' + IntToStr(FSequenceNo));
end;

procedure TLeaksAndPasses.ThisFailsDueSetUpLeak;
begin
  Check(FSequenceNo = 2,
    'SequenceNo should be 2 in ThisPassesAfterSetUpLeak but was ' + IntToStr(FSequenceNo));
end;

procedure TLeaksAndPasses.ThisLeaksOnRun;
begin
  Check(FSequenceNo = 3,
    'SequenceNo should be 3 in ThisLeaksOnRun but was ' + IntToStr(FSequenceNo));
  FRunObject := TObject.Create;
  Check(Assigned(FRunObject), 'Never fails');
end;

procedure TLeaksAndPasses.ThisPassesAfterRun;
begin
  Check(FSequenceNo = 4,
    'SequenceNo should be 4 in ThisPassesAfterRun but was ' + IntToStr(FSequenceNo));
  Check(Assigned(FRunObject), 'Never fails');
end;

procedure TLeaksAndPasses.ThisFailsDueTearDownLeak;
begin
  Check(FSequenceNo = 5,
    'SequenceNo should be 5 in ThisPassesBeforeTearDownLeak but was ' + IntToStr(FSequenceNo));
end;

procedure TLeaksAndPasses.ThisPassesAfterPriorTearDownLeak;
begin
  Check(FSequenceNo = 6,
    'SequenceNo should be 6 in ThisPassesAfterPriorTearDownLeak but was ' + IntToStr(FSequenceNo));
end;

procedure TLeaksAndPasses.ThisLeaksOnRunA;
begin
  Check(FSequenceNo = 7,
    'SequenceNo should be 7 in ThisLeaksOnRun but was ' + IntToStr(FSequenceNo));
  FRunObjectA := TObject.Create;
  Check(Assigned(FRunObjectA), 'Never fails');
end;

procedure TLeaksAndPasses.ThisLeaksOnRunB;
begin
  Check(FSequenceNo = 8,
    'SequenceNo should be 8 in ThisLeaksOnRun but was ' + IntToStr(FSequenceNo));
  FRunObjectB := TObject.Create;
  Check(Assigned(FRunObjectB), 'Never fails');
end;

procedure TLeaksAndPasses.ThisPassesAfterSucessiveLeaks;
begin
  Check(FSequenceNo = 9,
    'SequenceNo should be 9 in ThisPassesAfterRun but was ' + IntToStr(FSequenceNo));
  Check(Assigned(FRunObjectA), 'Never fails');
  Check(Assigned(FRunObjectB), 'Never fails');
end;

{ TTestLeakHandled }

procedure TTestAllProcsRunAfterLeakyProcRuns.SetUp;
begin
  FTestProject := nil;
  FLeaksAndPasses := nil;
end;

procedure TTestAllProcsRunAfterLeakyProcRuns.TearDown;
begin
  FTestProject.ExecutionControl.CurrentTest := nil;
  FLeaksAndPasses := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestAllProcsRunAfterLeakyProcRuns.TestAllProcsInTestCaseRunAfterLeakyProc;
var
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TLeaksAndPasses.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 9, 'Count <> 9 Count = ' +
    IntToStr(LCount));

  LExecControl := FTestProject.ExecutionControl;
  LExecControl.FailsOnMemoryLeak := True;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  FTestProject.Run(LExecControl);
  Check(LExecControl.ErrorCount = 0,
    'ErrorCount count should be 0 but was ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.FailureCount = 5,
    'FailureCount count should be 5 but was ' + IntToStr(LExecControl.FailureCount));
  Check(LExecControl.WarningCount = 0,
    'WarningCount count should be 0 but was ' + IntToStr(LExecControl.WarningCount));
  Check(LExecControl.ExecutionCount = 9,
    'Execution count should be 9 but was ' + IntToStr(LExecControl.ExecutionCount));
end;

{ TAllowedAndDisallowedLeakyTests }

procedure TAllowedAndDisallowedLeakyTests.SetUpOnce;
begin
  FreeAndNil(FObj1);
end;

procedure TAllowedAndDisallowedLeakyTests.TearDownOnce;
begin
  FreeAndNil(FObj1);
  FreeAndNil(FObj2);
  FreeAndNil(FObj3);
  FreeAndNil(FObj4);
end;

procedure TAllowedAndDisallowedLeakyTests.Proc1AllowedLeakPasses;
begin
  AllowedMemoryLeakSize := 16;
  FObj1 := TObject.Create;
  Check(Assigned(FObj1), 'Should exist');
end;

procedure TAllowedAndDisallowedLeakyTests.Proc2NotAllowedLeakFails;
begin
  FObj2 := TObject.Create;
  Check(Assigned(FObj2), 'Should exist');
end;

procedure TAllowedAndDisallowedLeakyTests.Proc3ArrayAllowdLeakPasses;
begin
  SetAllowedLeakArray([16]);
  FObj3 := TObject.Create;
  Check(Assigned(FObj3), 'Should exist');
end;

procedure TAllowedAndDisallowedLeakyTests.Proc4NoArrayAllowedLeakFails;
begin
  FObj4 := TObject.Create;
  Check(Assigned(FObj4), 'Should exist');
end;

{ TTestAllowedLeaksGetZerored }

procedure TTestAllowedLeaksGetZerored.SetUp;
begin
  FTestProject := nil;
  FAllowedAndDisallowedLeakyTests := nil;
end;

procedure TTestAllowedLeaksGetZerored.TearDown;
begin
  FTestProject.ExecutionControl.CurrentTest := nil;
  FAllowedAndDisallowedLeakyTests := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestAllowedLeaksGetZerored.CheckAllowedLeakSizesGetZeroedBetweenTests;
var
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TAllowedAndDisallowedLeakyTests.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 4, 'Count <> 4 Count = ' +
    IntToStr(LCount));

  LExecControl := FTestProject.ExecutionControl;
  LExecControl.FailsOnMemoryLeak := True;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  FTestProject.Run(LExecControl);
  Check(LExecControl.ErrorCount = 0,
    'ErrorCount count should be 0 but was ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.FailureCount = 2,
    'FailureCount count should be 2 but was ' + IntToStr(LExecControl.FailureCount));
  Check(LExecControl.WarningCount = 2,
    'WarningCount count should be 0 but was ' + IntToStr(LExecControl.WarningCount));
  Check(LExecControl.ExecutionCount = 4,
    'Execution count should be 9 but was ' + IntToStr(LExecControl.ExecutionCount));
end;

{$ENDIF}

{ TTestFullTestCaseCalled }

procedure TTestFullTestCaseCalled.SetUpOnce;
begin
  FSetupOnceCalled := True;
end;

procedure TTestFullTestCaseCalled.SetUp;
begin
  FSetupCalled := True;
end;

procedure TTestFullTestCaseCalled.TestExecutesNamedMethodSetup;
begin
  CheckTrue(FSetupOnceCalled, 'SetupOncedCalled not called');
  CheckTrue(FSetupCalled, 'Setup not called');
end;

procedure TestIProjectManager.SetUp;
begin
  FIProjectManager := TProjectManager.Create;
end;

procedure TestIProjectManager.TearDown;
begin
  FIProjectManager := nil;
end;


procedure TestIProjectManager.TestProjectManagerCreates;
var
  LReturnValue: Integer;
  AName: string;
begin
  AName := '';
  Check(Assigned(FIProjectManager), 'FIProjectManager was not assigned');
  LReturnValue := FIProjectManager.FindProjectID(AName);
  Check(LReturnValue = -1,
    'LReturnValue should be -1 but is ' + IntToStr(LReturnValue));
end;

{ TestRegisterTestToProjectDefault }

procedure TestRegisterTestToProjectDefault.SetUp;
begin
  FTestProject := nil;
end;

procedure TestRegisterTestToProjectDefault.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TestRegisterTestToProjectDefault.TestProjectManagerOnNilITest;
var
  LTest: ITestCase;
begin
  LTest := nil;
  TestFramework.RegisterTest(LTest);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'ProjectManager should now return nil');
  FTestProject := nil;
  FTestProject := TestFramework.TestProject(0);
  Check(FTestProject = nil, 'ProjectManager should now return nil');
end;

procedure TestRegisterTestToNamedProject.SetUp;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TestRegisterTestToNamedProject.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TestRegisterTestToNamedProject.TestProjectRegistersDefaultProject;
begin
  TestProjectRegistersDefaultProjectMethod;
end;

procedure TestRegisterTestToNamedProject.TestProjectRegistersNamedProject;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('ZYXWVU', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
//  Check(FTestProject.DisplayedName = DefaultProject,
//    'TestProject name should be DefaultProject but is ' + FTestProject.DisplayedName);

  // Now get hold of the named project reference
  FTestProject := TestFramework.TestProject((TestFramework.TestProject.Manager as IProjectManager).FindProjectID('ZYXWVU'));
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'ZYXWVU',
    'TestProject name should be ZYXWVU but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID,
    'Incorrect ProjectID. Should be ' + IntToStr(FTestProject.ProjectID) +
      ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(Assigned(LTest),
    'LTest should not be nil for TTestCase3. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestProjectRegistersSplitNamedProject;
var
  LTest: ITest;
  LCount: Integer;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('ZYX WVU', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'ZYX WVU',
    'TestProject name should be ZYX WVU but is ' + FTestProject.DisplayedName);

  // Now get hold of the named project reference
  FTestProject := TestFramework.TestProject((TestFramework.TestProject.Manager as IProjectManager).FindProjectID('ZYX WVU'));
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'ZYX WVU',
    'TestProject name should be ZYX WVU but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(Assigned(LTest),
    'LTest should not be nil for TTestCase3. NextEnabledProc failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.ProjectID = FTestProject.ProjectID, 'Incorrect ProjectID. Should be ' +
    IntToStr(FTestProject.ProjectID) + ' but was ' + IntToStr(LTest.ProjectID));
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestProjectRegistersDefaultProjectMethod;
var
  LTest: ITest;
  LCount: Integer;
  LProjectID: Integer;
  LProjectsManager: IProjectManager;
  LName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  LName := ExtractFileName(ParamStr(0));
  Check(FTestProject.DisplayedName = LName,
    'TestProject name should be ' + LName + ' but is ' + FTestProject.DisplayedName);

  LProjectsManager := (FTestProject.Manager as IProjectManager);
  LProjectID := LProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(LProjectID);

  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = LName,
    'TestProject name should be ' + LName + ' but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'LTest should not be nil for TestProcedure1');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'LTest should not be nil for TestProcedure2');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil for TestProcedure1');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil for TestProcedure2');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestProjectsRegistersDefaultAndNamedProjectMethod;
var
  LTest: ITest;
  LCount: Integer;
  LProjectID: Integer;
  LProjectsManager: IProjectManager;
  LName: string;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('UVWXYZ', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = DefaultProject,
    'TestProject name should be TTestProject but is ' + FTestProject.DisplayedName);
  LName := ExtractFileName(ParamStr(0));
  Check(FTestProject.ParentPath = LName,
    'TestProject ParentPath should be ' + LName + ' but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong name returned. Should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase2');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for ATestProcedure. FindNextEnableProc failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat iteration using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong name returned. Should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase2');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for ATestProcedure. FindNextTest failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Now conduct tests on named project
  LProjectsManager := (FTestProject.Manager as IProjectManager);
  LProjectID := LProjectsManager.FindProjectID('UVWXYZ');
  FTestProject := TestFramework.TestProject(LProjectID);

  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'UVWXYZ',
    'TestProject name should be UVWXYZ but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextEnableProc failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextEnableProc failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestProjectsRegistersDefaultAndNamedProject;
begin
  TestProjectsRegistersDefaultAndNamedProjectMethod;
end;

procedure TestRegisterTestToNamedProject.
  TestDifferentNamedProjectsRegisterSameNamedTestSuitesSeparately;
var
  LTest: ITest;
  LCount: Integer;
  LProjectID: Integer;
  LProjectsManager: IProjectManager;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', 'ABCDEF', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project1', 'ABCDEF', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = DefaultProject,
    'TestProject name should be TTestProject but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 1, 'Count <> 1 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'ABCDEF',
    'Wrong name returned. Should be ABCDEF but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');

  LProjectsManager := (FTestProject.Manager as IProjectManager);
  LProjectID := LProjectsManager.FindProjectID('Project1');
  FTestProject := TestFramework.TestProject(LProjectID);

  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'Project1',
    'TestProject name should be TProject1 but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'ABCDEF',
    'Wrong name returned. Should be ABCDEF but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat using FindNextTest
  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));
  Check(LTest.DisplayedName = 'ABCDEF',
    'Wrong name returned. Should be ABCDEF but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TTestTestCase3. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestSingleNamedProjectRegistersSameNamedTestSuitesTogether;
var
  LTest: ITest;
  LCount: Integer;
  LProjectID: Integer;
  LProjectsManager: IProjectManager;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project1', 'ABCDEF', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project1', 'ABCDEF', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = DefaultProject,
    'TestProject name should be Project1 but is ' + FTestProject.DisplayedName);

  LProjectsManager := (FTestProject.Manager as IProjectManager);
  LProjectID := LProjectsManager.FindProjectID('Project1');
  FTestProject := TestFramework.TestProject(LProjectID);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  Check(LTest.DisplayedName = 'ABCDEF',
    'Wrong name returned. Should be ABCDEF but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestSuite), 'Named tests must be TTestSuites');

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong name returned. Should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named test must be TTestCase');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for ATestProcedure. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned, should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned, should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  // Repeat
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest; //This has already been tested.
  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for ATestProcedure. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

procedure TestRegisterTestToNamedProject.TestNamedProjectRegistersDifferentNamedTestSuitesSeparately;
var
  LTest: ITest;
  LCount: Integer;
  LProjectID: Integer;
  LProjectsManager: IProjectManager;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('Project1', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project1', TTestCase3.Suite);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'Project1',
    'TestProject name should be ' + 'Project1' + ' but is ' + FTestProject.DisplayedName);

  LProjectsManager := (FTestProject.Manager as IProjectManager);
  LProjectID := LProjectsManager.FindProjectID('Project1');
  FTestProject := TestFramework.TestProject(LProjectID);

  Check(FTestProject <> nil, 'Test project must now exist');
  Check(FTestProject.DisplayedName <> '',
    'TestProject name should not be empty string');
  Check(FTestProject.DisplayedName = 'Project1',
    'TestProject name should be Project1 but is ' + FTestProject.DisplayedName);

  LTest := FTestProject.FindFirstTest;
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  Check(Assigned(LTest), 'LTest should not be nil');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong name returned. Should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named test must be TTestCase2');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned. Should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil');
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named test must be TTestCase');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'LTest should not be nil');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');
end;

{==============================================================================}
{ TTestEnableState }

function TTestEnableState.GetIniFileNextLine(const AFileName: string): string;
begin
  Result := '';
  FLine := '';
  if not FileExists(AFileName) then
  begin
    FFileOpened := False;
    Exit;
  end;

  if not FFileOpened then
  begin
    try
      AssignFile(FIniFile, AFileName);
      {$IFDEF CLR}
        Borland.Delphi.System.Reset(FIniFile);
      {$ELSE}
        System.Reset(FIniFile);
      {$ENDIF}
      FFileOpened := True;
    except
    end;
  end;

  if FFileOpened and not EOF(FIniFile) then
    ReadLn(FIniFile, FLine);
  Result := FLine;
end;

procedure TTestEnableState.SetUp;
begin
  FLine := '';
  if FFileOpened then
  try
    FFileOpened := False;
    CloseFile(FIniFile);
  except
  end;

  FProjectsManager := nil;
  RemoveProjectManager;
  FPath := ''; // TODO: LocalAppDataPath;
  FPathName := FPath + 'fptest.ini';
  SysUtils.DeleteFile(FPathName);
end;

procedure TTestEnableState.TearDown;
begin
  FPath := '';
  FLine := '';
  if FFileOpened then
  try
    FFileOpened := False;
    CloseFile(FIniFile);
  except
  end;

  SysUtils.DeleteFile(FPathName);
  FPathName := '';
  FProjectsManager := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

{ TestSaveEnableState }

procedure TestSaveEnableState.CallSaveConfigurationWithNoProjectsRegistered;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  FProjectsManager := TProjectManager.Create;
  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(not FileExists(FPathName), 'File should not be created');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithOneTestRegisteredEnabled;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  Check(LTest.DisplayedName = 'TTestCase2', 'Wrong ITest returned');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'ATestProcedure', 'Wrong ITest returned');
  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should be empty but was ' + FLine);
end;

procedure TestSaveEnableState.CallSaveConfigurationWithOneTestRegisteredDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  LTest.Enabled := False;
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest = nil, 'An EnabledProc should not be found');
  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should be empty');
  Check(FLine = '[Test.' + LExeName + '.TTestCase2]',
    'Should read [Test.' + LExeName + '.TTestCase2] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'ATestProcedure=0',
    'Should read ATestProcedure=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestsRegisteredEnabled;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase3.Suite); // Two procedures
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, '2nd EnabledProc not found');

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should be empty but was ' + FLine);
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestsRegistered1stDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase3.Suite); // Two procedures
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  LTest.Enabled := False;
  Check(LTest <> nil, 'An EnabledProc not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, '2nd EnabledProc not found');

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest = nil, 'Should not find another enabled test');

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.TTestCase3]',
    'Should read [Test.' + LExeName + '.TTestCase3] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestProcedure1=0',
    'Should read TestProcedure1=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestsRegistered2ndDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase3.Suite); // Two procedures
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, '2nd EnabledProc not found');
  LTest.Enabled := False;

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest = nil, 'Should not find another enabled test');

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.TTestCase3]',
    'Should read [Test.' + LExeName + '.TTestCase3] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestProcedure2=0',
    'Should read TestProcedure2=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithOneTestSuiteRegisteredEnabled;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('ATestSuite', TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'First ITest not found');

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should be empty but was ' + FLine);
end;

procedure TestSaveEnableState.CallSaveConfigurationWithOneTestSuiteRegisteredDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('ATestSuite', TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil, 'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'ATestSuite',
    'Wrong TestName should be ATestSuite but is ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + ']',
    'Should read [Test.' + LExeName + '] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'ATestSuite=0', 'Should read ATestSuite=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestSuitesRegistered2ndDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  TestFramework.RegisterTest('TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestSuite2');
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + ']',
    'Should read [Test.' + LExeName + '] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestSuite2=0', 'Should read TestSuite2=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestSuitesRegistered2ndTestCaseDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  TestFramework.RegisterTest('TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3');
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.TestSuite2]',
    'Should read [Test.' + LExeName + '.TestSuite2] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TTestCase3=0', 'Should read TTestCase3=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveEnableState.CallSaveConfigurationWithTwoTestSuitesRegistered2ndTestProcDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  TestFramework.RegisterTest('TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong Test should be TestProcedure2');
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.TestSuite2.TTestCase3]',
    'Should read [Test.' + LExeName + '.TestSuite2.TTestCase3] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestProcedure2=0',
    'Should read TestProcedure2=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

{ TestSaveProjectEnableState }

procedure TestSaveProjectEnableState.CallSaveConfigurationWithOneRegisteredProjectDisabled;
var
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('NamedProject', TTestCase3.Suite);

  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID(DefaultProject{'NamedProject'});
  FTestProject := TestFramework.TestProject(FProjectID);   // Retrieve the default project
  Check(FTestProject <> nil, 'Project must be assigned after registering TTestCase2');

  FTestProject.Enabled := False;
  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + ']',
  'Should read [Test.' + LExeName + '] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile line should not be empty');
  Check(FLine =
    DefaultProject + '=0', 'Should read Default Project=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveProjectEnableState.CallSaveConfigurationWith2ndRegisteredProjectDisabled;
var
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('NamedProject', TTestCase3.Suite);

  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('NamedProject');
  FTestProject := TestFramework.TestProject(FProjectID);  // Retrieve the default project
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase3');

  Check(FTestProject.DisplayedName = 'NamedProject',
    'Wrong name, should be NamedProject but was ' + FTestProject.DisplayedName);
  Check(FTestProject.DisplayedName = 'NamedProject',
    'Retrieved wrong project, name = ' + FTestProject.DisplayedName);
  FTestProject.Enabled := False;
  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + ']',
    'Should read [Test.' + LExeName + '] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'NamedProject=0', 'Should read NamedProject=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestSaveProjectEnableState.CallSaveConfigurationWithAllNamedProjectTestsDisabled;
var
  LTest: ITest;
  LExeName: string;
begin
  LExeName := ExtractFileName(ParamStr(0));
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('NamedProject', TTestCase3.Suite);

  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('NamedProject');
  FTestProject := TestFramework.TestProject(FProjectID);  // Retrieve the default project
  Check(FTestProject <> nil, 'Project must be assigned after registering TTestCase3');

  Check(FTestProject.DisplayedName = 'NamedProject',
    'Wrong display name, should be NamedProject but was ' + FTestProject.DisplayedName);
  FTestProject.Enabled := False;
  LTest := FTestProject.FindFirstTest;
  while LTest <> nil do
  begin
    LTest.Enabled := False;
    LTest := FTestProject.FindNextTest;
  end;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FileExists(FPathName), 'File should have been created');

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + ']',
    'Should read [Test.' + LExeName + '] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'NamedProject=0', 'Should read NamedProject=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.NamedProject]',
    'Should read [Test.' + LExeName + '.NamedProject] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TTestCase3=0', 'Should read TTestCase3=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = '[Test.' + LExeName + '.NamedProject.TTestCase3]',
    'Should read [Test.' + LExeName + '.NamedProject.TTestCase3] but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestProcedure1=0',
    'Should read TestProcedure1=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine <> '', 'IniFile should not be empty');
  Check(FLine = 'TestProcedure2=0',
    'Should read TestProcedure2=0 but read ' + FLine);

  GetIniFileNextLine(FPathName);
  Check(FLine = '', 'IniFile should now be empty');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadSingleTestMethod;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong testname, should be ATestProcedure but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadDisablesSecondTestMethod;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong testname, should be TestProcedure2 but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadDisablesSingleTestCase;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong testname, should be TTestCase2 but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong testname, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(LTest.Enabled = True, 'LoadConfiguration disabled test');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadDisablesSecondTestCase;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.RegisterTest(TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong testname, should be TTestCase3 but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong testname, should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(LTest.Enabled = True, 'LoadConfiguration disabled test');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadDisablesSingleTestSuite;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TestSuite1',
    'Wrong testname, should be TestSuite1 but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An TTestCase not found');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong testname, should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(LTest.Enabled = True, 'LoadConfiguration disabled test');
end;

procedure TestLoadConfigurationDisablesTests.TestLoadDisablesSecondTestSuite;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  TestFramework.RegisterTest('TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestSuite2',
    'Wrong testname, should be TestSuite2 but was ' + LTest.DisplayedName);
  LTest.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'SaveConfiguration altered enable status');
  LTest.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Enabled = False, 'LoadConfiguration failed to apply disable');

  CheckTrue(FTestProject.FindFirstTest.Enabled, 'TestSuite1 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TTestCase2 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'ATestProcedure should be True');
  CheckFalse(FTestProject.FindNextTest.Enabled, 'TestSuite2 should be False');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TTestCase3 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure1 be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure2 be True');
end;

{ TestLoadConfigurationExcludesTests }

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSingleTestMethod;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong testname, should be ATestProcedure but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'SaveConfiguration altered excluded status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply excluded');
end;

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSecondTestMethod;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  LTest := FTestProject.FindNextEnabledProc;
  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong testname, should be TestProcedure2 but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'SaveConfiguration altered Excluded status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply Excluded');
end;

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSingleTestCase;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'First ITest not found');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong testname, should be TTestCase2 but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'SaveConfiguration altered enable status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply disable');
  Check(LTest.Enabled = True, 'LoadExcludedConfiguration disabled TestCase');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong testname, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(LTest.Excluded = False, 'LoadConfiguration excluded test');
  Check(LTest.Enabled = True, 'LoadExcludedConfiguration disabled test');
end;

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSecondTestCase;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestCase2.Suite);
  TestFramework.RegisterTest(TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong testname, should be TTestCase3 but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'SaveConfiguration altered excluded status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply excluded');
  Check(LTest.Enabled = True, 'LoadConfiguration disabled 2nd TestCase');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An EnabledProc not found');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong testname, should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(LTest.Excluded = False, 'LoadConfiguration excluded test');
  Check(LTest.Enabled = True, 'LoadConfiguration disabled test');
end;

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSingleTestSuite;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TestSuite1',
    'Wrong testname, should be TestSuite1 but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Enabled = True, 'SaveConfiguration altered enable status');
  Check(LTest.Excluded = True, 'SaveConfiguration altered excluded status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply excluded');

  LTest := FTestProject.FindNextTest;
  Check(LTest <> nil, 'An TTestCase not found');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong testname, should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(LTest.Enabled = True, 'LoadConfiguration disabled test');
  Check(LTest.Excluded = False, 'LoadConfiguration excluded test');
end;

procedure TestLoadConfigurationExcludesTests.TestLoadExcludesSecondTestSuite;
var
  LTest: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest('TestSuite1', TTestCase2.Suite);
  TestFramework.RegisterTest('TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil,
    'Project must be assigned after registering TTestCase2');
  LTest := FTestProject.FindFirstTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = 'TestSuite2',
    'Wrong testname, should be TestSuite2 but was ' + LTest.DisplayedName);
  LTest.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'SaveConfiguration altered excluded status');
  LTest.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(LTest.Excluded = True, 'LoadConfiguration failed to apply excluded');

  CheckFalse(FTestProject.FindFirstTest.Excluded, 'TestSuite1 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TTestCase2 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'ATestProcedure should be False');
  CheckTrue(FTestProject.FindNextTest.Excluded, 'TestSuite2 should be True');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TTestCase3 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure1 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure2 should be False');
end;

{ TestLoadConfigurationDisablesProjects }

procedure TestLoadConfigurationDisablesProjects.TestLoadDisablesSingleTestProject;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', 'TestSuite1', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('', 'TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil, 'Project must be assigned after registering TTestCase2');
  FTestProject.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FTestProject.Enabled = False, 'SaveConfiguration altered enable status');
  FTestProject.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(FTestProject.Enabled = False, 'LoadConfiguration failed to apply disable');

  CheckTrue(FTestProject.FindFirstTest.Enabled, 'TestSuite1 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TTestCase2 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'ATestProcedure should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestSuite2 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TTestCase3 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure1 be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure2 be True');
end;

procedure TestLoadConfigurationDisablesProjects.TestLoadDisablesSecondTestProject;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', 'TestSuite1', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project2', 'TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('Project2');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(Assigned(FTestProject),
    'Project must be assigned after registering TTestCase2');
  FTestProject.Enabled := False;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FTestProject.Enabled = False, 'SaveConfiguration altered enable status');
  FTestProject.Enabled := True;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(FTestProject.Enabled = False, 'LoadConfiguration failed to apply disable');

  CheckTrue(FTestProject.FindFirstTest.Enabled, 'TestSuite2 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TTestCase3 should be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure1 be True');
  CheckTrue(FTestProject.FindNextTest.Enabled, 'TestProcedure2 be True');
end;


{ TestLoadConfigurationExcludesProjects }

procedure TestLoadConfigurationExcludesProjects.TestLoadExcludedSingleTestProject;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', 'TestSuite1', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('', 'TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(FTestProject <> nil, 'Project must be assigned after registering TTestCase2');
  FTestProject.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FTestProject.Excluded = True, 'SaveConfiguration altered excluded status');
  FTestProject.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(FTestProject.Excluded = True, 'LoadConfiguration failed to apply excluded');

  CheckFalse(FTestProject.FindFirstTest.Excluded, 'TestSuite1 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TTestCase2 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'ATestProcedure should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestSuite2 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TTestCase3 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure1 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure2 should be False');
end;

procedure TestLoadConfigurationExcludesProjects.TestLoadExcludedSecondTestProject;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.ProjectRegisterTest('', 'TestSuite1', TTestCase2.Suite);
  TestFramework.ProjectRegisterTest('Project2', 'TestSuite2', TTestCase3.Suite);
  FProjectsManager := (TestFramework.TestProject.Manager as IProjectManager);
  FProjectID := FProjectsManager.FindProjectID('Project2');
  FTestProject := TestFramework.TestProject(FProjectID);
  Check(Assigned(FTestProject),
    'Project must be assigned after registering TTestCase2');
  FTestProject.Excluded := True;

  FProjectsManager.SaveConfiguration(FPathName, False, False);
  Check(FTestProject.Excluded = True, 'SaveConfiguration altered excluded status');
  FTestProject.Excluded := False;
  FProjectsManager.LoadConfiguration(FPathName, False, False);
  Check(FTestProject.Excluded = True, 'LoadConfiguration failed to apply excluded');

  CheckFalse(FTestProject.FindFirstTest.Excluded, 'TestSuite2 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TTestCase3 should be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure1 be False');
  CheckFalse(FTestProject.FindNextTest.Excluded, 'TestProcedure2 be False');
end;

{ TStatusMessageCallbackTest }

procedure TStatusMessageTests.AssignStatusMessage1;
begin
  Check(GetStatus = '', 'Status message should be empty but was ' + GetStatus);
  Status('ABCDEF');
  Check(GetStatus = 'ABCDEF'#$D#$A,
    'Status message should be ABCDEF#$D#$A but was ' + GetStatus);

  Status('GHIJKL');
  Check(GetStatus = 'ABCDEF'#$D#$A'GHIJKL'#$D#$A,
    'Status message should be ABCDEF#$D#$AGHIJKL#$D#$A but was ' + GetStatus);
end;

procedure TStatusMessageTests.AssignStatusMessage2;
begin
  Check(GetStatus = '', 'Status message should be empty but was ' + GetStatus);
  Status('MNOPQR');
  Check(GetStatus = 'MNOPQR'#$D#$A,
    'Status message should be MNOPQR but was ' + GetStatus);
  Status('STUVWX');
  Check(GetStatus = 'MNOPQR'#$D#$A'STUVWX'#$D#$A,
    'Status message should be MNOPQR#$D#$ASTWVWX#$D#$A but was ' + GetStatus);
end;

{ TTestStatusMessageCallback }

procedure TTestStatusMessageCallback.SetUp;
begin
  FITest := nil;
  TestFramework.RegisterTest(TStatusMessageTests.Suite);
  FTestProject := TestFramework.TestProject;
  FCalledBackMessages := TStringList.Create;
end;

procedure TTestStatusMessageCallback.TearDown;
begin
  FreeAndNil(FCalledBackMessages);
  FITest := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestStatusMessageCallback.StatusMsgListener(const ATest: ITest;
  const AStatusMsg: string);
begin
  FCalledBack := True;
  FCalledBackMessages.Add(AStatusMsg);
end;

procedure TTestStatusMessageCallback.CheckCallGetStatusMsg;
var
  LReturnStatus: TExecutionStatus;
  LExecControl: ITestExecControl;
begin
  Check(Assigned(FTestProject), 'FTestProject = nil');
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LExecControl.StatusMsgUpdater := nil;
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = {$IFDEF FASTMM} _Warning, {$ELSE} _Passed, {$ENDIF}
    'Error occurred running AssignStatusMessage ExecStatus = ' +
       GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  FITest := FTestProject.FindFirstTest;
  FITest := FTestProject.FindNextEnabledProc;
  Check(FITest.GetStatus = 'ABCDEF'#$D#$A'GHIJKL'#$D#$A,
    'Status message should be ABCDEF#$D#$AGHIJKL#$D#$A but was ' + FITest.GetStatus);

  FITest := FTestProject.FindNextEnabledProc;
  Check(Assigned(FITest), 'FITest = nil');
  Check(FITest.DisplayedName = 'AssignStatusMessage2',
    'Test name should be AssignStatusMessage2 but was ' +
    FITest.DisplayedName);
  Check(FITest.GetStatus = 'MNOPQR'#$D#$A'STUVWX'#$D#$A,
    'Incorrect GetStatus result, function was ' + FITest.GetStatus);
end;

procedure TTestStatusMessageCallback.CheckCallBackOnAssignStatusMsg;
var
  LReturnStatus: TExecutionStatus;
  LExecControl: ITestExecControl;
begin
  Check(Assigned(FTestProject), 'FTestProject = nil');
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LExecControl.StatusMsgUpdater := StatusMsgListener;
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = {$IFDEF FASTMM} _Warning, {$ELSE} _Passed, {$ENDIF}
    'Error occurred running AssignStatusMessage ExecStatus = ' +
       GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  Check(FCalledBack, 'Status failed to call back with message');
  Check(FCalledBackMessages.Count = 4,
    'Count should be 4 but was ' + IntToStr(FCalledBackMessages.Count));
  Check(FCalledBackMessages.Strings[0] = 'ABCDEF',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[0]);
  Check(FCalledBackMessages.Strings[1] = 'GHIJKL',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[1]);
  Check(FCalledBackMessages.Strings[2] = 'MNOPQR',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[2]);
  Check(FCalledBackMessages.Strings[3] = 'STUVWX',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[3]);
end;

{ TTestStatusMessageAutoCallback }

procedure TTestStatusMessageAutoCallback.SetUp;
begin
  FCalledBack  := False;
  FTestProject := nil;
  FITest       := nil;
  FCalledBackMessages := TStringList.Create;
end;

procedure TTestStatusMessageAutoCallback.TearDown;
begin
  FreeAndNil(FCalledBackMessages);
  FTestProject := nil;
  FITest       := nil;
  RemoveProjectManager;
end;

procedure TTestStatusMessageAutoCallback.StatusMsgListener(const ATest: ITest;
  const AStatusMsg: string);
begin
  FCalledBack := True;
  FCalledBackMessages.Add(AStatusMsg);
end;

procedure TTestStatusMessageAutoCallback.CheckAssignStatusMsgCallsback;
// This checks that TestProject.Run(FExecControl) causes the ITest's
// Stringlist clears so repeaded "Run's" dont accumulate test endlessly.
var
  LReturnStatus: TExecutionStatus;
  LExecControl: ITestExecControl;
begin
  TestFramework.RegisterTest(TStatusMessageTests.Suite);
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  LExecControl.StatusMsgUpdater := StatusMsgListener;

  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = {$IFDEF FASTMM} _Warning, {$ELSE} _Passed, {$ENDIF}
    'Execution failed with return status of ' +
       GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  Check(FCalledBack, 'Status failed to call out of Project');
  Check(FCalledBackMessages.Count = 4,
    'Count should be 4 but was ' + IntToStr(FCalledBackMessages.Count));
  Check(FCalledBackMessages.Strings[0] = 'ABCDEF',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[0]);
  Check(FCalledBackMessages.Strings[1] = 'GHIJKL',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[1]);
  Check(FCalledBackMessages.Strings[2] = 'MNOPQR',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[2]);
  Check(FCalledBackMessages.Strings[3] = 'STUVWX',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[3]);

  FCalledBack  := False;
  FCalledBackMessages.Clear;
  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = {$IFDEF FASTMM} _Warning, {$ELSE} _Passed, {$ENDIF}
    'Execution failed with return status of ' +
       GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  Check(FCalledBack, 'Status failed to call out of Project');
  Check(FCalledBackMessages.Count = 4,
    'Count should be 4 but was ' + IntToStr(FCalledBackMessages.Count));
  Check(FCalledBackMessages.Strings[0] = 'ABCDEF',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[0]);
  Check(FCalledBackMessages.Strings[1] = 'GHIJKL',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[1]);
  Check(FCalledBackMessages.Strings[2] = 'MNOPQR',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[2]);
  Check(FCalledBackMessages.Strings[3] = 'STUVWX',
    'Called Back Message incorect, was ' + FCalledBackMessages.Strings[3]);
end;


{ TDecoratedTest }

procedure TDecoratedTest.SetUpOnce;
begin
  Inc(DSetUpOnceCalled);
end;

procedure TDecoratedTest.SetUp;
begin
  Inc(DSetUpCalled);
end;

procedure TDecoratedTest.TestPass1;
begin
  Inc(DPass1Called);
  Check(DPass1Called = 1, 'Wrong Count');
end;

procedure TDecoratedTest.TestPass2;
begin
  Inc(DPass2Called);
  Check(DPass2Called = 1, 'Wrong Count');
end;

procedure TDecoratedTest.TearDown;
begin
  Inc(DTearDownCalled);
end;

procedure TDecoratedTest.TearDownOnce;
begin
  Inc(DTearDownOnceCalled);
end;

{ TTestDecoratesTest }

procedure TTestDecoratesTest.SetUpOnce;
begin
  Inc(GSetUpOnceCalled);
end;

procedure TTestDecoratesTest.SetUp;
begin
  Inc(GSetUpCalled);
end;

procedure TTestDecoratesTest.TearDown;
begin
  Inc(GTearDownCalled);
end;

procedure TTestDecoratesTest.TearDownOnce;
begin
  Inc(GTearDownOnceCalled);
end;

{ TTestDecoatorDecorates }

procedure TTestDecoratorDecorates.CheckDecoratorPrefixAndNames;
var
  LTest: ITest;
begin
  TestFramework.RegisterTest(TTestDecoratesTest.Suite('AAA', TDecoratedTest.Suite));
  FTestProject := TestFramework.TestProject;

  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = '(d) TTestDecoratesTest',
    'Wrong testname, should be <(d) TTestDecoratesTest> but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'A test named AAA should be assigned');
  Check(LTest.DisplayedName = 'AAA',
    'Wrong testname, should be <AAA> but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'A test named TestPass1 should be assigned');
  Check(LTest.DisplayedName = 'TestPass1',
    'Wrong testname, should be <TestPass1> but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'A test named TestPass2 should be assigned');
  Check(LTest.DisplayedName = 'TestPass2',
    'Wrong testname, should be <TestPass2> but was ' + LTest.DisplayedName);
end;

procedure TTestDecoratorDecorates.CheckExecutionSequence;
var
  LReturnStatus: TExecutionStatus;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  GSetUpOnceCalled := 0;
  GSetUpCalled := 0;
  GTearDownCalled := 0;
  GTearDownOnceCalled := 0;

  DSetUpOnceCalled := 0;
  DSetUpCalled := 0;
  DTearDownCalled := 0;
  DTearDownOnceCalled := 0;

  DPass1Called := 0;
  DPass2Called := 0;

  TestFramework.RegisterTest(TTestDecoratesTest.Suite('XYZ', TDecoratedTest.Suite));
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));

  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Passed, 'Decorated tests should have passed');
  Check(GSetUpOnceCalled = 1,    'GSetUpOnceCalled <> 1');
  Check(GSetUpCalled = 1,        'GSetUpCalled <> 1');

  Check(DSetUpOnceCalled = 1,    'DSetUpOnceCalled <> 1');
  Check(DSetUpCalled = 2,        'DSetUpCalled <> 2');
  Check(DPass1Called = 1,        'DPass1Called <> 1');
  Check(DPass2Called = 1,        'DPass2Called <> 1');
  Check(DTearDownCalled = 2,     'DTearDownCalled <> 2');
  Check(DTearDownOnceCalled = 1, 'DTearDownOnceCalled <> 1');

  Check(GTearDownCalled = 1,     'GTearDownCalled <> 1');
  Check(GTearDownOnceCalled = 1, 'GTearDownOnceCalled <> 1');
end;

procedure TTestDecoratorDecorates.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestDecoratorDecorates.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

{ TTestSuiteDecorator }

procedure TTestSuiteDecorator.SetUp;
begin
  inherited;
end;

{ TTestDecoratorRegistersTestSuite }

procedure TTestDecoratorRegistersTestSuite.SetUp;
begin
  FProject := nil;
end;

procedure TTestDecoratorRegistersTestSuite.TearDown;
begin
  FProject := nil;
  RemoveProjectManager;
end;

procedure TTestDecoratorRegistersTestSuite.CheckRegistersTestSuite;
var
  LTest: ITest;
  LCount: Integer;
begin
  FProject := TestFramework.Projects;
  Check(FProject = nil, 'Project should be nil at test start');
  TestFramework.RegisterTest(TTestSuiteDecorator.Suite(TestFramework.TTestSuite.Suite('JKL', [TTestCase1.Suite, TTestCase3.Suite])));
  FProject := TestFramework.Projects;
  Check(Assigned(FProject), 'Project should exist');

  LCount := FProject.CountEnabledTests;
  Check(LCount = 3, 'Count of tests should be 3 but was ' + IntToStr(LCount));

  LTest := FProject.FindFirstTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = '(d) TTestSuiteDecorator', 'Test name should be (d) TTestSuiteDecorator but was ' + LTest.DisplayedName);

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'JKL', 'Test name should be JKL but was ' + LTest.DisplayedName);

  LCount := LTest.Count;
  Check(LCount = 3, 'Count of tests should be 3 but was ' + IntToStr(LCount));

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'TTestCase1', 'Test name should be TTestCase1 but was ' + LTest.DisplayedName);

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'ATestProcedure', 'Test name should be ATestProcedure but was ' + LTest.DisplayedName);

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'TTestCase3', 'Test name should be TTestCase3 but was ' + LTest.DisplayedName);

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'TestProcedure1', 'Test name should be TestProcedure1 but was ' + LTest.DisplayedName);

  LTest := FProject.FindNextTest;
  Check(Assigned(LTest), 'Test should exist');
  Check(LTest.DisplayedName = 'TestProcedure2', 'Test name should be TestProcedure2 but was ' + LTest.DisplayedName);

end;

{ TVerifyDecoratorHandlesPeripheryFailures }

procedure TVerifyDecoratorHandlesPeripheryFailures.SetUp;
begin
  Assert((TestSetUpData = nil), 'TestSetUpData should be nil on entry');
  TestSetUpData := (TDecoratorSharedData.Create as IDecoratorSharedData);
  (TestSetUpData as IDecoratorSharedData).RanCount := 1;
  FAnExecControl := TestFramework.TestExecControl;
  FAnExecControl.TestSetUpData := TestSetUpData;
end;

procedure TVerifyDecoratorHandlesPeripheryFailures.TearDown;
begin
  TestSetUpData := nil;
  FAnExecControl := nil;
end;

{ TDecoratorSharedData }

function TDecoratorSharedData.get_RanCount: Integer;
begin
  Result := FRanCount;
end;

procedure TDecoratorSharedData.set_RanCount(const Value: Integer);
begin
  FRanCount := Value;
end;


{ TDecoratedTestWithSetUpOnceFailure }

procedure TDecoratedTestWithSetUpOnceFailure.ProcPasses;
begin
  Check(True);
end;

procedure TDecoratedTestWithSetUpOnceFailure.SetUpOnce;
begin
  raise TestFramework.EDUnitException.Create('Deliberate failure in SetUpOnce');
end;

{ TDecoratedTestWithSetUpFailure }

procedure TDecoratedTestWithSetUpFailure.ProcPasses;
begin
  Check(True);
end;

procedure TDecoratedTestWithSetUpFailure.SetUp;
begin
  raise TestFramework.EDUnitException.Create('Deliberate failure in SetUp');
end;

{ TDecoratedTestWithTearDownFailure }

procedure TDecoratedTestWithTearDownFailure.ProcPasses;
begin
  Check(True);
end;

procedure TDecoratedTestWithTearDownFailure.TearDown;
begin
  raise TestFramework.EDUnitException.Create('Deliberate failure in TearDown');
end;

{ TDecoratedTestWithTearDownOnceFailure }

procedure TDecoratedTestWithTearDownOnceFailure.ProcPasses;
begin
  Check(True);
end;

procedure TDecoratedTestWithTearDownOnceFailure.TearDownOnce;
begin
  raise TestFramework.EDUnitException.Create('Deliberate failure in TearDownOnce');
end;


{ TVerifyDecoratorWithOtherFailuresTests }

procedure TVerifyDecoratorReportsPeripheryFailures.SetUp;
begin
  TestFramework.RegisterTest(TVerifyDecoratorHandlesPeripheryFailures.Suite('A suite of decorated tests',
    [TDecoratedTestWithSetUpOnceFailure.Suite,
     TDecoratedTestWithSetUpFailure.Suite,
     TDecoratedTestWithTearDownFailure.Suite,
     TDecoratedTestWithTearDownOnceFailure.Suite]));
  FProject := TestFramework.Projects;
end;

procedure TVerifyDecoratorReportsPeripheryFailures.TearDown;
begin
  FProject := nil;
  RemoveProjectManager;
end;

procedure TVerifyDecoratorReportsPeripheryFailures.VerifyDecoratedTestsFlagErrorsInPeripheryProcs;
var
  LExecControl: ITestExecControl;
begin
  Check(Assigned(FProject), 'Project should exist');
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := True; //FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  Check(FProject.Run(LExecControl) = _Error,
    'VerifyDecoratedTestsFlagErrorsInPeripheryProcs should report error');
  Check(LExecControl.ExecutionCount = 2,
    'Execution count should be 2 but was ' + IntToStr(LExecControl.ExecutionCount));
  Check(LExecControl.ErrorCount = 4,
    'Error count should be 4 but was ' + IntToStr(LExecControl.ExecutionCount));
end;

{ -------------------------------------------------}
{ TTestCountedTestHalts }

procedure TTestCountedTestHalts.SetUp;
begin
  HaltOnError := True;
end;

procedure TTestRepeatTest.SetUp;
begin
  FTestProject := nil;
end;

procedure TTestRepeatTest.TearDown;
begin
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestRepeatTest.CheckFailsAndExecutesNTimes;
var
  LTest: ITest;
  LReturnStatus: TExecutionStatus;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  GlobalCount := 0;
  TestFramework.RegisterTest(TTestRepeatedTests.Suite(TCountedTestsWithFailures.Suite, 5));
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TTestRepeatedTests',
    'Wrong testname, should be <TTestRepeatedTests> but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = '5 * TCountedTestsWithFailures',
    'Wrong testname, should be 5 * TCountedTestsWithFailures but was ' + LTest.DisplayedName);

  LCount := FTestProject.Count;
  Check(LCount = 35, 'Count <> 35 Count = ' + IntToStr(LCount));

  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Failed, 'Decorated tests should have passed');
  Check(LExecControl.ExecutionCount = 35, 'All 7 tests should have been called 5 times for a total of 35');
end;

procedure TTestRepeatTest.CheckFailsAndHalts;
var
  LTest: ITest;
  LReturnStatus: TExecutionStatus;
  LCount: Integer;
  LExecControl: ITestExecControl;
begin
  GlobalCount := 0;
  TestFramework.RegisterTest(TTestCountedTestHalts.Suite(TCountedTestsWithFailures.Suite, 5));
  FTestProject := TestFramework.TestProject;
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  LTest := FTestProject.FindFirstTest;
  Check(LTest.DisplayedName = 'TTestCountedTestHalts',
    'Wrong testname, should be <TTestCountedTestHalts> but was ' + LTest.DisplayedName);

  LTest := FTestProject.FindNextTest;
  Check(LTest.DisplayedName = '5 * TCountedTestsWithFailures',
    'Wrong testname, should be 5 * TCountedTestsWithFailures but was ' + LTest.DisplayedName);

  LCount := FTestProject.Count;
  Check(LCount = 35, 'Count <> 35 Count = ' + IntToStr(LCount));

  LReturnStatus := FTestProject.Run(LExecControl);
  Check(LReturnStatus = _Failed, 'Decorated tests should have passed');
  Check(LExecControl.ExecutionCount = 7,
    'All 7 tests should have been called 1 time only but were called ' +
      IntToStr(LExecControl.ExecutionCount));
  Check(GlobalCount = 1, 'GlobalCount should be 1 but was ' + IntToStr(GlobalCount) );
end;

{ TCountedTestsWithFailures }

procedure TCountedTestsWithFailures.SetUpOnce;
begin
  Inc(GlobalCount);
end;

procedure TCountedTestsWithFailures.PassesA;
begin
  Check(True);
end;

procedure TCountedTestsWithFailures.ShouldFailOnFirstExecution;
begin
  Check(GlobalCount <> 1, 'Failed when GlobalCount = ' + IntToStr(GlobalCount));
end;

procedure TCountedTestsWithFailures.PassesB;
begin
  Check(True);
end;

procedure TCountedTestsWithFailures.ShouldFailOnThirdExecution;
begin
  Check(GlobalCount <> 3, 'Failed when GlobalCount = ' + IntToStr(GlobalCount));
end;

procedure TCountedTestsWithFailures.PassesC;
begin
  Check(True);
end;

procedure TCountedTestsWithFailures.ShouldFailOnFifthExecution;
begin
  Check(GlobalCount <> 5, 'Failed when GlobalCount = ' + IntToStr(GlobalCount));
end;

procedure TCountedTestsWithFailures.PassesD;
begin
  Check(True);
end;

{ TestMultiProject }

procedure TestMultiProject.SetUp;
begin
  FAnExecControl := nil;
  FTestProject := nil;
  FProjectsManager := nil;
  FITestedProjects := TestRegisterTestToNamedProject.Create;
  FMultiProjectSuite := nil;
  FITestedProjects.SetUp;
end;

procedure TestMultiProject.TearDown;
begin
  if Assigned(FITestedProjects) then
    FITestedProjects.TearDown;
  FMultiProjectSuite := nil;
  FITestedProjects := nil;
  FAnExecControl := nil;
  FTestProject := nil;
  FProjectsManager := nil;
  RemoveProjectManager;
end;

procedure TestMultiProject.CheckNoMultiProjectOnDefaultOnly;
var
  LCount: Integer;
begin
  FITestedProjects.TestProjectRegistersDefaultProjectMethod;
  FProjectsManager := TestFrameWork.TestProject.Manager as IProjectManager;
  LCount := FProjectsManager.Count;
  Check(LCount = 1, 'Count of projects should be 1 but was ' +
    IntToStr(LCount));
  Check(not Assigned(FProjectsManager.Projects), 'Projects should not be assigned');
end;

procedure TestMultiProject.CheckMultiProjectGetsGivenExeName;
var
  LCount: Integer;
begin
  FITestedProjects.TestProjectsRegistersDefaultAndNamedProjectMethod;
  FProjectsManager := TestFrameWork.TestProject.Manager as IProjectManager;
  LCount := FProjectsManager.Count;
  Check(LCount = 2, 'Count of projects should be 2 but was ' +
    IntToStr(LCount));
  FMultiProjectSuite := FProjectsManager.Projects;
  Check(Assigned(FProjectsManager.Projects), 'Projects should now be assigned');
  Check(FProjectsManager.Projects.DisplayedName = ExtractFileName(ParamStr(0)),
    'Projects name error = ' + FProjectsManager.Projects.DisplayedName);
  Check(FProjectsManager.Project[0].DisplayedName = DefaultProject,
    'DefaultProject name error: = ' + FProjectsManager.Project[0].DisplayedName);
  Check(FProjectsManager.Project[0].ParentPath = ExtractFileName(ParamStr(0)),
    'ParentPath name error: = ' + FProjectsManager.Project[0].ParentPath);
  Check(FMultiProjectSuite.ParentPath = '',
    'Top level MultiProjectSuite ParentPath should be empty but is ' + FMultiProjectSuite.ParentPath);
end;

procedure TestMultiProject.CheckMultiProjectCountsAndIteratesTests;
var
  LCount: Integer;
  LTest: ITest;
begin
  CheckMultiProjectGetsGivenExeName;
  FMultiProjectSuite := FProjectsManager.Projects;
  LCount := FMultiProjectSuite.Count;
  Check(LCount = 3,
    'Count should be 3 but is ' + IntToStr(LCount));
  // I cannot remember the purpose of having difference counts.
  LTest := FMultiProjectSuite.FindFirstTest;
  Check(Assigned(LTest), 'First test should not be nil');
  Check(Supports(LTest, ITestProject), 'First test should be an ITestProject');
  Check(LTest.DisplayedName = DefaultProject,
    'Wrong name returned. Should be ' + DefaultProject + ' but was ' + LTest.DisplayedName);

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest), 'TestCase should not be nil');
  Check(LTest.DisplayedName = 'TTestCase2',
    'Wrong name returned. Should be TTestCase2 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase2');

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest), 'First test should not be nil');
  Check(Supports(LTest, ITestProject), 'First test should be an ITestProject');
  Check(LTest.DisplayedName = 'UVWXYZ',
    'Wrong name returned. Should be UVWXYZ but was ' + LTest.DisplayedName);
  LCount := (LTest as ITestProject).Count;
  Check(LCount = 2, 'Count <> 2 Count = ' + IntToStr(LCount));

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest), 'First testcase should not be nil');
  Check(LTest.DisplayedName = 'TTestCase3',
    'Wrong name returned. Should be TTestCase3 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestCase), 'Named tests must be TTestCase3');

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextTest;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextTest;
  Check(not Assigned(LTest), 'Last + 1 test should be nil');
end;

procedure TestMultiProject.CheckMultiProjectCountAndIterateTestCases;
var
  LCount: Integer;
  LTest: ITest;
begin
  CheckMultiProjectGetsGivenExeName;
  FMultiProjectSuite := FProjectsManager.Projects;
  LCount := FMultiProjectSuite.Count;
  Check(LCount = 3,
    'Count should be 3 but is ' + IntToStr(LCount));
  // I cannot remember the purpose of having difference counts.
  LTest := FMultiProjectSuite.FindFirstTest;
  Check(Assigned(LTest), 'First test should not be nil');
  Check(Supports(LTest, ITestProject), 'First test should be an ITestProject');
  LCount := (LTest as ITestProject).Count;
  Check(LCount = 1, 'Count <> 1 Count = ' + IntToStr(LCount));

  LTest := FMultiProjectSuite.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for ATestProcedure. NextEnabledProc failed');
  Check(LTest.DisplayedName = 'ATestProcedure',
    'Wrong name returned, should be ATestProcedure but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure1. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure1',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextEnabledProc;
  Check(Assigned(LTest),
    'LTest should not be nil for TestProcedure2. FindNextTest Failed');
  Check(LTest.DisplayedName = 'TestProcedure2',
    'Wrong name returned. Should be TestProcedure1 but was ' + LTest.DisplayedName);
  Check(Supports(LTest, ITestMethod), 'Named test procedure must be ITestMethod');

  LTest := FMultiProjectSuite.FindNextEnabledProc;
  Check(not Assigned(LTest), 'Last + 1 test should be nil');
end;

procedure TestMultiProject.CheckMultiProjectExecControl;
begin
  CheckMultiProjectGetsGivenExeName;
  FMultiProjectSuite := FProjectsManager.Projects;
  FAnExecControl := FMultiProjectSuite.ExecutionControl;
  Check(Assigned(FAnExecControl), 'ITestExecControl is nil');
  Check(Supports(FAnExecControl, ITestExecControl), 'Returned value is not an ITestExecControl');
end;

procedure TestMultiProject.CheckMultiProjectCountSetsDepths;
var
  LCount: Integer;
  LTest: ITest;
begin
  CheckMultiProjectExecControl;
  LCount := FMultiProjectSuite.Count;
  Check(LCount = 3, 'Count of test methods should be 3 but is ' + IntToStr(LCount));
  LTest := FMultiProjectSuite;
  Check(LTest.Depth = 0, LTest.DisplayedName + ' Depth Depth should be 0 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindFirstTest;
  Check(LTest.Depth = 1, LTest.DisplayedName + ' Depth should be 1 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(LTest.Depth = 2, LTest.DisplayedName + ' Depth should be 2 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(LTest.Depth = 3, LTest.DisplayedName + ' Depth should be 3 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;

  Check(LTest.Depth = 1, LTest.DisplayedName + ' Depth should be 1 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(LTest.Depth = 2, LTest.DisplayedName + ' Depth should be 2 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(LTest.Depth = 3, LTest.DisplayedName + ' Depth should be 3 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(LTest.Depth = 3, LTest.DisplayedName + ' Depth should be 3 but is ' + IntToStr(LTest.Depth));
  LTest := FMultiProjectSuite.FindNextTest;
  Check(not Assigned(LTest), 'LTest should be nil');
end;

procedure TestMultiProject.CheckMultiProjectRunsSingleThreaded;
var
  LReturnStatus: TExecutionStatus;
  LExecControl: ITestExecControl;
begin
  LExecControl := TestFramework.TestExecControl;
  {$IFDEF FASTMM}
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}
  CheckMultiProjectExecControl;
  LReturnStatus := FMultiProjectSuite.Run(LExecControl);
    Check(LReturnStatus = _Warning,
      'MultiProject should return _Warning but returned ' +
        GetEnumName(TypeInfo(TExecutionStatus), Ord(LReturnStatus)));
  Check(LExecControl.ExecutionCount = 3,
    'ExecutionCount should equal 3 but is ' + IntToStr(LExecControl.ExecutionCount));
  Check(LExecControl.FailureCount = 0,
    'FailureCount should equal 0 but is ' + IntToStr(LExecControl.FailureCount));
  Check(LExecControl.ErrorCount = 0,
    'ErrorCount should equal 0 but is ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.WarningCount = 3,
    'WarningCount should equal 3 but is ' + IntToStr(LExecControl.WarningCount));
end;

{ TContainsSuccessiveLeaks }

{$IFDEF FASTMM}
var
  USize1: Integer;
  USize2: Integer;
  USize3: Integer;
  UObj1: TObject;
  UObj2: TObject;
  UObj3: TObject;
  UObj4: TObject;
  UObj5: TObject;
  UObj6: TObject;

procedure TContainsSuccessiveLeaks.SetUpOnce;
var
  LMemLeakMonitor: IDUnitMemLeakMonitor;
begin
  USize1 := 0;
  USize2 := 0;
  USize3 := 0;
  LMemLeakMonitor := TestFramework.MemLeakMonitor;
  UObj1 := TObject.Create;
  LMemLeakMonitor.MemLeakDetected(F1ObjsSize);
  FreeAndNil(UObj1);
  F2ObjsSize := F1ObjsSize * 2;
  F3ObjsSize := F1ObjsSize * 3;
end;

procedure TContainsSuccessiveLeaks.TearDownOnce;
begin
  FreeAndNil(UObj6);
  FreeAndNil(UObj5);
  FreeAndNil(UObj4);
  FreeAndNil(UObj3);
  FreeAndNil(UObj2);
  FreeAndNil(UObj1);
end;

procedure TContainsSuccessiveLeaks.Leak1Obj;
var
  LMemLeakMonitor: IDUnitMemLeakMonitor;
  LSize: integer;
  LTotal: Integer;
begin
  SetAllowedLeakArray([F1ObjsSize, F2ObjsSize, F3ObjsSize]);
  LMemLeakMonitor := TestFramework.MemLeakMonitor;
  UObj1 := TObject.Create;
  LTotal := F1ObjsSize;
  try
    CheckFalse(LMemLeakMonitor.MemLeakDetected(LTotal, False, LSize),
      '1 Obj leak not Allowed' + IntToStr(LSize));
  finally
    LMemLeakMonitor := nil;
  end;
  Check(LSize = LTotal, 'Leak size difference = ' + IntToStr(LTotal - LSize));
  USize1 := LSize;
end;

procedure TContainsSuccessiveLeaks.Leak2Obj;
var
  LMemLeakMonitor: IDUnitMemLeakMonitor;
  LSize: integer;
  LTotal: Integer;
begin
  SetAllowedLeakArray([F1ObjsSize, F2ObjsSize, F3ObjsSize]);
  LMemLeakMonitor := TestFramework.MemLeakMonitor;
  UObj2 := TObject.Create;
  UObj3 := TObject.Create;
  LTotal := F2ObjsSize;
  try
    CheckFalse(LMemLeakMonitor.MemLeakDetected(LTotal, False, LSize),
      '2 Obj leak not Allowed' + IntToStr(LSize));
  finally
    LMemLeakMonitor := nil;
  end;
  Check(LSize = LTotal, 'Leak size difference = ' + IntToStr(LTotal - LSize));
  USize2 := LSize;
end;

procedure TContainsSuccessiveLeaks.Leak3Obj;
var
  LMemLeakMonitor: IDUnitMemLeakMonitor;
  LSize: integer;
  LTotal: Integer;
begin
  SetAllowedLeakArray([F1ObjsSize, F2ObjsSize, F3ObjsSize]);
  LMemLeakMonitor := TestFramework.MemLeakMonitor;
  UObj4 := TObject.Create;
  UObj5 := TObject.Create;
  UObj6 := TObject.Create;
  LTotal := F3ObjsSize;
  try
    CheckFalse(LMemLeakMonitor.MemLeakDetected(LTotal, False, LSize),
      '3 Obj leak not Allowed' + IntToStr(LSize));
  finally
    LMemLeakMonitor := nil;
  end;
  Check(LSize = LTotal, 'Leak size difference = ' + IntToStr(LTotal - LSize));
  USize3 := LSize;
end;

{ TTestSuccessiveLeaksReport }

procedure TTestSuccessiveLeaksReport.SetUp;
begin
  UObj1 := nil;
  UObj2 := nil;
  UObj3 := nil;
  UObj4 := nil;
  UObj5 := nil;
  UObj6 := nil;
  FSuccessiveLeaks := nil;
  FTestProject := nil;
end;

procedure TTestSuccessiveLeaksReport.TearDown;
begin
  FreeAndNil(UObj6);
  FreeAndNil(UObj5);
  FreeAndNil(UObj4);
  FreeAndNil(UObj3);
  FreeAndNil(UObj2);
  FreeAndNil(UObj1);
  FTestProject.ExecutionControl.CurrentTest := nil;
  FSuccessiveLeaks := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestSuccessiveLeaksReport.TestSuccessiveLeaksGetReported;
var
  LCount: Integer;
  LExecControl: ITestExecControl;
  LTest: ITest;
  LTest1: ITest;
  LTest2: ITest;
  LTest3: ITest;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTests('', [TContainsSuccessiveLeaks.Suite]);
  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' +
    IntToStr(LCount));

  LExecControl := FTestProject.ExecutionControl;
  LExecControl.FailsOnMemoryLeak := True;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  FTestProject.Run(LExecControl);
  Check(LExecControl.ErrorCount = 0,
    'ErrorCount count should be 0 but was ' + IntToStr(LExecControl.ErrorCount));
  Check(LExecControl.FailureCount = 0,
    'FailureCount count should be 0 but was ' + IntToStr(LExecControl.FailureCount));
  Check(LExecControl.WarningCount = 3,
    'WarningCount count should be 3 but was ' + IntToStr(LExecControl.WarningCount));
  Check(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));

  LTest := FTestProject.FindFirstTest;
  Check(LTest <> nil, 'TestCase should not be nil');
  Check(LTest.AllowedMemoryLeakSize =  0,
    'TestCase size should be 0 but was ' + IntToStr(LTest.AllowedMemoryLeakSize));

  LTest1 := FTestProject.FindNextTest;
  Check(LTest1 <> nil, 'First test should not be nil');
  Check(LTest1.AllowedMemoryLeakSize =  USize1,
    'First size should be ' + IntToStr(USize1) + ' but was ' + IntToStr(LTest1.AllowedMemoryLeakSize));

  LTest2 := FTestProject.FindNextTest;
  Check(LTest2 <> nil, 'Second test should not be nil');
  Check(LTest2.AllowedMemoryLeakSize =  USize2,
    'Second size should be ' + IntToStr(USize2) + ' but was ' + IntToStr(LTest2.AllowedMemoryLeakSize));

  LTest3 := FTestProject.FindNextTest;
  Check(LTest3 <> nil, 'Third test should not be nil');
  Check(LTest3.AllowedMemoryLeakSize =  USize3,
    'Third size should be ' + IntToStr(USize3) + ' but was ' + IntToStr(LTest3.AllowedMemoryLeakSize));

  LTest := FTestProject.FindNextTest;
  Check(LTest = nil, 'After last test should be nil');
end;
{$ENDIF}

{ TTestSuiteHasSetUpFailure }

procedure TTestSuiteHasSetUpFailure.XRuns;
begin
  Check(True);
end;

procedure TTestSuiteHasSetUpFailure.YRuns;
begin
  Check(True);
end;

procedure TTestSuiteHasSetUpFailure.SetUp;
begin
  Assert(False, 'SetUp must Fail');
end;

{ TTestSuiteHasTearDownFailure }

procedure TTestSuiteHasTearDownFailure.XRuns;
begin
  Check(True);
end;

procedure TTestSuiteHasTearDownFailure.YRuns;
begin
  Check(True);
end;

procedure TTestSuiteHasTearDownFailure.TearDown;
begin
  Assert(False, 'TearDown must Fail');
end;

{ TTestHandlesTestSuiteNonMethodFailures }

procedure TTestHandlesTestSuiteNonMethodFailures.SetUp;
begin
  FTestProject := nil;
  FAnExecControl := nil;
end;

procedure TTestHandlesTestSuiteNonMethodFailures.TearDown;
begin
  FTestProject := nil;
  FAnExecControl := nil;
  RemoveProjectManager;
end;

procedure TTestHandlesTestSuiteNonMethodFailures.VerifySetUpFailureInTTestSuiteHandled;
var
  LTest: ITest;
  LCount: Integer;
  LResult: TExecutionStatus;
begin
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestSuiteHasSetUpFailure.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSuiteHasSetUpFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
  Check(Supports(LTest, ITestSuite), 'Wrong class created');
  (LTest as ITestSuite).AddTest(TTestSimplePass2.Suite);

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSuiteHasSetUpFailure', 'Wrong name = ' +
    LTest.DisplayedName);
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(not Assigned(LTest), 'Should be nil');

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := True; //FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  FTestProject.CountEnabledTests;
  LResult := FTestProject.Run(FAnExecControl);
  Check(LResult = _Error,
    'Result should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LResult)));

  FTestProject.Reset; //This resets all iterators
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 0, 'Execution Count <> 0 Count = ' +
    IntToStr(LCount));

  Check(FAnExecControl.ErrorCount = 3,
    'ErrorCount should be 3 but was ' + IntToStr(FAnExecControl.ErrorCount));

  LTest := FTestProject.FindNextTest;
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.ExecStatus = _Ready,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest = nil, 'Must not be any more tests');

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSuiteHasSetUpFailure', 'Wrong name = ' +
    LTest.DisplayedName);
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
end;

procedure TTestHandlesTestSuiteNonMethodFailures.VerifyTearDownFailureInTTestSuiteHandled;
var
  LTest: ITest;
  LCount: Integer;
  LResult: TExecutionStatus;
begin
  {$IFNDEF CLR}
  AllowedMemoryLeakSize := 24;
  {$ENDIF}
  FTestProject := TestFramework.TestProject;
  Check(FTestProject = nil, 'Must be nil at start of test');
  TestFramework.RegisterTest(TTestSuiteHasTearDownFailure.Suite);

  FTestProject := TestFramework.TestProject;
  Check(FTestProject <> nil, 'Test project must now exist');
  LCount := FTestProject.Count;
  Check(LCount = 2, 'Count <> 2 Count = ' +
    IntToStr(LCount));

  LTest := FTestProject.FindFirstTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSuiteHasTearDownFailure', 'Wrong test, name = ' +
    LTest.DisplayedName);

  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');
  Check(Supports(LTest, ITestSuite), 'Wrong class created');
  (LTest as ITestSuite).AddTest(TTestSimplePass2.Suite);

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  LTest := FTestProject.FindNextTest;
  Check(not Assigned(LTest), 'Should be nil');

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong test, name = ' +
    LTest.DisplayedName);

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Ready, 'Status should be _Ready');

  FAnExecControl := FTestProject.ExecutionControl;
  {$IFDEF FASTMM}
  FAnExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  {$ENDIF}

  FTestProject.CountEnabledTests;
  LResult := FTestProject.Run(FAnExecControl);
  Check(LResult = _Error,
    'Result should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LResult)));

  FTestProject.Reset; //This resets all iterators
  LCount := FAnExecControl.ExecutionCount;
  Check(LCount = 3, 'Execution Count <> 3 Count = ' +
    IntToStr(LCount));

  Check(FAnExecControl.ErrorCount = 3,
    'ErrorCount should be 3 but was ' + IntToStr(FAnExecControl.ErrorCount));

  LTest := FTestProject.FindNextTest;
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.ExecStatus = _Error,
    'Test should have been _Error but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(Assigned(LTest), 'Could not locate an executable Test');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong test, name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed,
    'Test should have been _Passed but was ' +
      GetEnumName(TypeInfo(TExecutionStatus), Ord(LTest.ExecStatus)));

  LTest := FTestProject.FindNextEnabledProc;
  Check(LTest = nil, 'Must not be any more tests');

  LTest := FTestProject.FindFirstTest; //This resets all iterators
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSuiteHasTearDownFailure', 'Wrong name = ' +
    LTest.DisplayedName);
  LCount := FTestProject.Count;
  Check(LCount = 3, 'Count <> 3 Count = ' + IntToStr(LCount));

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'XRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'YRuns', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a TestCase');
  Check(LTest.DisplayedName = 'TTestSimplePass2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Error, 'Status should be _Error');

  LTest := FTestProject.FindNextTest;
  Check(Assigned(LTest), 'Could not locate a Test Method');
  Check(LTest.DisplayedName = 'SimpleMethod2', 'Wrong name = ' +
    LTest.DisplayedName);
  Check(LTest.ExecStatus = _Passed, 'Status should be _Passed');
end;


{ TLongModeExitsOnFail }

procedure TLongModeExitsOnFail.ShouldExitOnFail;
begin
  Status('Start');
  EarlyExitCheck(False, 'Long mode should exit on fail');
  Status('Next');
  Check(False, 'CheckExit long mode failed to exit early on fail');
  Status('Bad');
end;

{ TLongModeNoExitOnPass }

procedure TLongModeNoExitOnPass.ShouldContinueOnPass;
begin
  Status('Start');
  EarlyExitCheck(True, 'CheckExit in long mode should continue and hit next Check');
  Status('Next');
  Check(False, 'CheckExit should allow code to hit this failure');
  Status('Bad');
end;

{ TExitsEarlyOnTrue }

procedure TExitsEarlyOnTrue.ShouldExitEarly;
begin
  Status('Start');
  EarlyExitCheck(True, 'Should exit before next check fails');
  Status('Next');
  Check(False, 'CheckExit failed to exit early as pass');
  Status('Bad');
end;

{ TNoExitOnFalse }

procedure TNoExitOnFalse.ShouldNotExitOnFalse;
begin
  Status('Start');
  EarlyExitCheck(False, 'CheckExit should continue and hit next Check');
  Status('Next');
  Check(False, 'CheckExit should allow code to hit this failure');
  Status('Bad');
end;

{ TEmptyCheckExit }

procedure TEmptyCheckExit.EmptyMethodReportsCHMsgOnFalse;
begin
  Status('Start');
  EarlyExitCheck(False, 'CheckExit error message');
  Status(ErrorMessage);
end;

{ TTestCheckExitBehaviour }

procedure TTestCheckExitBehaviour.ExecStatusUpdater(const ATest: ITest);
begin
  Assert(Assigned(ATest), 'Test nil in ExecStatusUpdater');
  if ATest.IsTestMethod then
    FErrorMsg := ATest.ErrorMessage;
end;

{ TPassCheckExit }

procedure TPassFollowsCheckExit.MethodHasPassingCheck;
begin
  Status('Start');
  EarlyExitCheck(False, 'CheckExit error message');
  Status('Next');
  Check(True);
  Status(ErrorMessage);
end;

{ TTwoPassesFollowsCheckExit }

procedure TTwoPassesFollowsCheckExit.MethodHasPassingChecks;
begin
  Status('Start');
  EarlyExitCheck(False, 'CheckExit error message1');
  Status('Next');
  EarlyExitCheck(False, 'CheckExit error message2');
  Check(True);
  Status(ErrorMessage);
end;

procedure TTestCheckExitBehaviour.SetUp;
begin
  inherited;
  FXC := nil;
  FStatusMsg := '';
  FErrorMsg := '';
  FTestProject := nil;
end;

procedure TTestCheckExitBehaviour.StatusMsgUpdater(const ATest: ITest;
                                                   const AStatusMsg: string);
begin
  FStatusMsg := AStatusMsg
end;

procedure TTestCheckExitBehaviour.TearDown;
begin
  inherited;
  FErrorMsg := '';
  FStatusMsg := '';
  FXC := nil;
  FTestProject := nil;
  RemoveProjectManager;
end;

procedure TTestCheckExitBehaviour.VerifyLongModeExitsOnFail;
begin
  TestFrameWork.RegisterTest(TLongModeExitsOnFail.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.InhibitSummaryLevelChecks := True;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.ExecStatusUpdater := ExecStatusUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('Start', FStatusMsg, 'Status message should only be Start');
  CheckEqualsString('Long mode should exit on fail', FErrorMsg, 'Error should have reported CheckExit error message');
  Check(FXC.CheckCalledCount = 1, 'Check called count should be 1 but was '+ IntToStr(FXC.CheckCalledCount));
  Check(FStatus = _Failed, 'In long mode CheckExit should exit on failure');
end;

procedure TTestCheckExitBehaviour.VerifyLongModeNoExitsOnPass;
begin
  TestFrameWork.RegisterTest(TLongModeNoExitOnPass.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.InhibitSummaryLevelChecks := True;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('Next', FStatusMsg, 'CheckExit should have only executed first statement');
  Check(FStatus = _Failed, 'CheckExit should have failed');
  Check(FXC.CheckCalledCount = 2, 'Check called count should be 2 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifyReportsAndFailsOnChecksPass;
begin
  TestFrameWork.RegisterTest(TPassFollowsCheckExit.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.ExecStatusUpdater := ExecStatusUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  Check(FStatus = _Failed, 'CheckExit should have failed');
  CheckEqualsString('CheckExit error message', FStatusMsg, 'Status should have reported CheckExit error message');
  CheckEqualsString('CheckExit error message', FErrorMsg, 'Error should have reported CheckExit error message');
  Check(FXC.CheckCalledCount = 2, 'Check called count should be 2 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifyExitsEarlyOnTrue;
begin
  TestFrameWork.RegisterTest(TExitsEarlyOnTrue.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('Start', FStatusMsg, 'CheckExit should have only executed first statement');
  Check((FStatus = _Passed) or (FStatus = _Warning),
    'CheckExit should pass or warn on mem use but not fail');
  Check(FXC.CheckCalledCount = 1, 'Check called count should be 1 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifyNoExitOnFalse;
begin
  TestFrameWork.RegisterTest(TNoExitOnFalse.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('Next', FStatusMsg, 'CheckExit should have only executed first statement');
  Check(FStatus = _Failed, 'CheckExit should have failed');
  Check(FXC.CheckCalledCount = 2, 'Check called count should be 2 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifyEmptyCEReportsAndFails;
begin
  TestFrameWork.RegisterTest(TEmptyCheckExit.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('CheckExit error message', FStatusMsg, 'Should show CheckExit error message ');
  Check(FStatus = _Failed, 'CheckExit should have failed');
  Check(FXC.CheckCalledCount = 1, 'Check called count should be 1 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifyFollowingFailReports;
begin
  TestFrameWork.RegisterTest(TNoExitOnFalse.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.ExecStatusUpdater := ExecStatusUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  CheckEqualsString('Next', FStatusMsg, 'Test should have executed CheckExit then Check(False) then exited');
  Check(FStatus = _Failed, 'CheckExit should have failed');
  CheckEqualsString('CheckExit should allow code to hit this failure', FErrorMsg, 'Incorrect error message');
  Check(FXC.CheckCalledCount = 2, 'Check called count should be 2 but was '+ IntToStr(FXC.CheckCalledCount));
end;

procedure TTestCheckExitBehaviour.VerifySerialCEsAllReportAndFails;
begin
  TestFrameWork.RegisterTest(TTwoPassesFollowsCheckExit.Suite);
  FTestProject := TestFrameWork.TestProject;
  FXC := TestFrameWork.TestExecControl;
  FXC.StatusMsgUpdater := StatusMsgUpdater;
  FXC.ExecStatusUpdater := ExecStatusUpdater;
  FXC.FailsOnNoChecksExecuted := True;
  FXC.InhibitStackTrace := True;
  FExecControl.InhibitStackTrace := True;
  FStatus := FTestProject.Run(FXC);
  Check(FStatus = _Failed, 'CheckExit should have failed');
  CheckEqualsString('CheckExit error message1.CheckExit error message2', FStatusMsg, 'Should show concatenated CheckExit error messages');
  CheckEqualsString('CheckExit error message1.CheckExit error message2', FErrorMsg, 'Error should show concatenated CheckExit error messages');
  Check(FXC.CheckCalledCount = 3, 'Check called count should be 3 but was '+ IntToStr(FXC.CheckCalledCount));
end;

initialization
  RefTestFramework.
  RegisterTests('TestTestProc',
                              [TTestITestProc.Suite,
                               TTestITestProcName.Suite,
                               TTestITestProcCreatesNamed.Suite]);

  RefTestFramework.
  RegisterTests('ValidateTestCaseBehavior',
                              [TTestITestCaseExists.Suite,
                               TTestNamedTestCase.Suite,
                               TTestTestCase1.Suite,
//                               TTestNonPublished.Suite,  //D2007 demands TTestProc has RTTI info turned on
                               TTestATTestCase.Suite,
                               TTest2TTestCase.Suite,
                               TTest3LevelsCase.Suite,
                               TVerifyOverriddenGetNameIsAccessed.Suite,
                               TTestTestCaseCanCountAndRetrieveList.Suite,
                               TTestCheckExitBehaviour.Suite]);

//  RefTestFramework.
//  RegisterTest('ValidateTestCaseBehavior', TTestRegisrationOfParamMethodTestCase.Suite);

  RefTestFramework.
  RegisterTests('VerifyAllChecksSignalPassAndFail',
                              [TTestCheckTests.Suite,
                               TTestChecksCount.Suite]);

  RefTestFramework.
  RegisterTests('ValidateTestSuiteBehaviour',
                              [TTestITestSuiteExists.Suite,
                               TTestITestSuiteAddMethods.Suite,
                               TTestCreateNamedTest.Suite,
                               TTestTestSuiteParentPathPropagates.Suite,
                               TTestITestSuiteRegistersTestCases.Suite,
                               TTestITestSuiteCanCountAndList.Suite,
                               TTestTTestITestSuiteAddsTestCases.Suite,
                               TTestParentPathPopulatesTests.Suite,
                               TVerifyGetNameInSuiteIsAccessed.Suite,
                               TTestLevelTestRunsFrom.Suite]);

  RefTestFramework.
  RegisterTests('VerifyLeagcyCode', [TTestLegacyRunTestMethod.Suite,
                                    TTestLegacyRunTestMethodCheckFails.Suite]);
  RefTestFramework.
  RegisterTests('ValidateTestProjectBehavior',
                              [TTestITestProjectExists.Suite,
                               TTestTestProjectBehaviorOnRegisterFirstTestCase.Suite,
                               TTestTestProjectAddMethods.Suite,
                               TTestTestProjectAddSuite.Suite]);

  RefTestFramework.
  RegisterTests('ValidateProjectManagerBehavior',
                              [TestIProjectManager.Suite,
                               TTestITestProjectIterator.Suite,
                               TTestITestCanRunProject.Suite,
                               TTestMultiProjectCount.Suite]);

  RefTestFramework.
  RegisterTests('TestErrorHandling',
                              [TTestTestCaseHandlesFailCalls.Suite,
                               TTestTestCaseHandlesHaltCall.Suite,
                               {$IFNDEF CLR}
                               TTestFailsOnOptimizedEmptyTest.Suite,
                               {$ENDIF}
                               TTestFailsOnCheckNotCalled.Suite,
                               TTestExceptionHandled.Suite,
                               TTestTearDownFailInTestSuite.Suite,
                               TTestPeripheryExceptions.Suite,
                               TTestTestCaseHandlesNonMethodFailures.Suite,
                               TTestHandlesTestSuiteNonMethodFailures.Suite,
                               TTestErrorReporting.Suite,
                               TTestExcludedTestReporting.Suite,
                               TTestHandlesBreakOnNotPassed.Suite]);

  RefTestFramework.
  RegisterTests('TestProjectRegistration',
                              [TestRegisterTestToProjectDefault.Suite,
                               TestRegisterTestToNamedProject.Suite]);

  RefTestFramework.
  RegisterTest('TestRecordElapsedTime', TTestsElapsedTimes.Suite);

  RefTestFramework.
  RegisterTest(TTestSetUpDataAccess.Suite);

  RefTestFramework.
  RegisterTests('TestStatusReports',
                              [TTestStatusMessageCallback.Suite,
                              TTestStatusMessageAutoCallback.Suite]);

  RefTestFramework.
  RegisterTests('TestSaveConfiguration',
                              [TestSaveEnableState.Suite,
                               TestSaveProjectEnableState.Suite]);

  RefTestFramework.
  RegisterTests('TestLoadConfiguration',
                              [TestLoadConfigurationDisablesTests.Suite,
                               TestLoadConfigurationDisablesProjects.Suite,
                               TestLoadConfigurationExcludesTests.Suite,
                               TestLoadConfigurationExcludesProjects.Suite]);

  RefTestFramework.
  RegisterTests('TestDecorator_Extentions',
                              [TTestDecoratorDecorates.Suite,
                              TTestRepeatTest.Suite,
                              TVerifyDecoratorReportsPeripheryFailures.Suite,
                              TTestDecoratorRegistersTestSuite.Suite]);

  {$IFDEF FASTMM}
    RefTestFramework.
    RegisterTests('TestMemoryMonitor',
                              [TTestMemoryMonitor.Suite,
                               TTestSuccessiveLeaksReport.Suite,
                               TTestAllProcsRunAfterLeakyProcRuns.Suite,
                               TTestAllowedLeaksGetZerored.Suite
                              ]);
  {$ENDIF}

  RefTestFramework.
  ProjectRegisterTest('Test Multiple Projects', TestMultiProject.Suite);

end.

