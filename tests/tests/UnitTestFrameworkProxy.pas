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

unit UnitTestFrameworkProxy;

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
  TestFrameworkIfaces,
  TestFrameworkProxyIfaces,
  RefTestFramework;

type
  TSimpleTestCase1 = class(TTestCase)
  published
    procedure TestPasses;
  end;

  TSimpleTestCase2 = class(TTestCase)
  published
    procedure FirstTestPasses;
    procedure SecondTestPasses;
  end;

  TTestProxyFrameworkRegistersProjects = class(TTestCase)
  private
    FProject1: ITestProject;
    FProject2: ITestProject;
    FTestFrameworkProxy: ITestProxy;
    FExeName: string;
    FDLLProject: ITestProject;
    FDLLName: string;
    FDLLFullPathAndName: string;

  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published
    procedure VerifyRegisteredProxyReturnsSameSingleAutoRegisteredEmptyProjectst;
    procedure VerifyRegisteredProxyReturnsSameSingleAutoRegisteredEmptyProject;
    procedure VerifyRegisteredProxyReturnsSameSingleAutoRegisteredSimpleProject;
    procedure VerifyRegisteredProxyReturnsTwoRegisteredSimpleProjects;
    {$ifndef FPC}
    procedure VerifyRegisteredProxyReturnsSameSingleCodeRegisteredDLL;
    procedure VerifyRegisteredProxyReturnsSameSingleAutoRegisteredDLL;
    procedure VerifyRegisteredProxyReturnsOneRegisteredProjectAndOneRegisteredDLL;
    procedure VerifyRegisteredProxyReturnsOneRegisteredDLLAndOneRegisteredProject;
    {$endif}
  end;

  {$ifdef fastmm}
    {$ifndef clr}
      {$ifndef VER130}
        {$IFNDEF VER140}
  TLeaksAndPasses = class(TTestCase)
  private
    FSetUpObject:    TObject;
    FRunObject:      TObject;
    FTearDownObject: TObject;
    FRunObjectA:     TObject;
    FRunObjectB:     TObject;
    FSequenceNo:     Integer;
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

  // Created after observing all tests following two detected memory leaks
  // withing a TTestCase class failed to run, when XMLListener is registered.

  TTestMultiLeakHandling = class(TTestCase)
  private
    FProject1: ITestProject;
    FTestFrameworkProxy: ITestProxy;
    FExeName: string;
    FTestResult: TTestResult;
    FXMLFile: string;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published
    procedure ValidateMultiLeakHandling;
    procedure ValidateXMLEffectOnMultiLeakHanding;
  end;
        {$ENDIF}
      {$endif}
    {$endif}
  {$endif}

  TTestBreakOnFailure = class(TTestCase)
   private
    FTestProject: ITestProject;
    FAnExecControl: ITestExecControl;
    FTestFrameworkProxy: ITestProxy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestsBreakOnFailure;
    {$IFDEF CLR}[Test]{$ENDIF}
    procedure VerifyTestsBreakOnException;
  end;

implementation
uses
  ProjectsManagerIface,
  TestFrameworkProxy,
  TestFramework,
  {$ifndef FPC}
  TestModules,
   {$IFNDEF VER130}
     {$IFNDEF VER140}
       XMLListener,
     {$ENDIF}
     Windows,
   {$ENDIF}
  {$ENDIF}
  SharedTestClasses,
  SysUtils;


{ TSimpleProject }

procedure TSimpleTestCase1.TestPasses;
begin
  Check(True);
end;

{ TSimpleTestCase2 }

procedure TSimpleTestCase2.FirstTestPasses;
begin
  Check(True);
end;

procedure TSimpleTestCase2.SecondTestPasses;
begin
  Check(True);
end;

{ TTestGetRegisteredTest }

procedure TTestProxyFrameworkRegistersProjects.SetUpOnce;
begin
  FExeName := ExtractFileName(ParamStr(0));
  FDLLName := 'SingleTestLibW32.dtl';
  FDLLFullPathAndName := ExtractFilePath(ParamStr(0)) + FDLLName;
end;

procedure TTestProxyFrameworkRegistersProjects.SetUp;
begin
  FProject1 := nil;
  FProject1 := TestFramework.TTestProject.Create;
  FProject1.ParentPath := FExeName;
  FDLLProject := nil;
end;

procedure TTestProxyFrameworkRegistersProjects.TearDown;
begin
  if Assigned(FProject1) then
    FProject1.ReleaseProxys;
  if Assigned(FProject2) then
    FProject2.ReleaseProxys;
  if Assigned(FDLLProject) then
    FDLLProject.ReleaseProxys;
  FProject1 := nil;
  FProject2 := nil;
  FDLLProject := nil;
  if Assigned(FTestFrameworkProxy) then
    FTestFrameworkProxy.ReleaseTests;
  FTestFrameworkProxy := nil;
  TestFramework.UnRegisterProjectManager;
  {$ifndef FPC}
  TestModules.UnloadTestModules;
  {$endif}
end;

procedure TTestProxyFrameworkRegistersProjects.TearDownOnce;
begin
  FExeName := '';
  FDLLName := '';
  FDLLFullPathAndName := '';
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsSameSingleAutoRegisteredEmptyProjectst;
begin
  FProject1 := nil;
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy = nil, 'Should be nil until project registered.');
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsSameSingleAutoRegisteredEmptyProject;
var
  LXYZ: string;
  LCount: Integer;
begin
  LXYZ := 'XYZ';
  FProject1.DisplayedName := LXYZ;
  TestFramework.RegisterProject(FProject1);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = LXYZ,
    'Returned project name should be XYZ but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = FExeName,
    'Parent path not set to exename but is <' + FTestFrameworkProxy.ParentPath + '>');
  Check(FTestFrameworkProxy.IsTestMethod = False, 'Project should not show as TestMethod');;
  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 0,
    'List of added projects should be 0 but was ' + IntToStr(LCount));
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 0,
    'EnabledTestCases should be 0 but was ' + IntToStr(LCount));
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsSameSingleAutoRegisteredSimpleProject;
var
  LCount: Integer;
begin
  FProject1.AddTest(TSimpleTestCase1.Suite);
  TestFramework.RegisterProject(FProject1);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = 'TTestProject',
    'Returned project name should be TTestProject but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = FExeName,
    'Parent path not set to exename but is <' + FTestFrameworkProxy.ParentPath + '>');
  Check(FTestFrameworkProxy.IsTestMethod = False, 'Project should not show as TestMethod');;
  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 1,
    'List of added projects should be 1 but was ' + IntToStr(LCount));
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 1,
    'EnabledTestCases should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsTwoRegisteredSimpleProjects;
var
  LCount: Integer;
  LPM: IProjectManager;
  LExecControl: ITestExecControl;
begin
  FProject1 := nil;
  FProject2 := nil;
  TestFramework.ProjectRegisterTest('', 'Suite1', TSimpleTestCase1.Suite);
  TestFramework.ProjectRegisterTest('2ndProject', 'Suite2', TSimpleTestCase2.Suite);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  LPM := TestFramework.Projects.Manager as IProjectManager;
  LExecControl := LPM.Projects.ExecutionControl;
  LPM.Projects.Run(LExecControl);

  EarlyExitCheck(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));

  // Failed final check. See if input conditions were correct
  LCount := LPM.Count;
  Check(LCount = 2,
    'Count of Projects tests should be 2 but was ' + IntToStr(LCount));
  FProject1 := LPM.Project[0];
  Check(Assigned(FProject1), 'Project at idx=0 ' + DefaultProject + 'should not be nil');
  LCount := FProject1.CountEnabledTests;
  Check(LCount = 1,
    'Count of enabled tests in Project1 should be 1 but was ' + IntToStr(LCount));

  FProject2 := LPM.Project[1];
  Check(Assigned(FProject2), 'Project at idx=1 2ndProject should not be nil');
  LCount := FProject2.CountEnabledTests;
  Check(LCount = 2,
    'Count of enabled tests in Project2 should be 2 but was ' + IntToStr(LCount));
  LCount := TestFramework.Projects.CountEnabledTests;
  Check(LCount = 3,
    'Count of enabled tests should be 3 but was ' + IntToStr(LCount));

// moved to top
// FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FExeName,
    'Returned project name should be ' + FExeName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = '',
    'Parent path should be empty but is <' + FTestFrameworkProxy.ParentPath + '>');
  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 2,
    'List of added projects should be 2 but was ' + IntToStr(LCount));
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 3,
    'EnabledTestCases should be 3 but was ' + IntToStr(LCount));


 // Moved to top and use checkExit
{
  LExecControl := LPM.Projects.ExecutionControl;
  LExecControl.ClearCounts;
  LPM.Projects.Run(LExecControl);
  Check(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));
}
end;

{$ifndef FPC}
procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsSameSingleCodeRegisteredDLL;
var
  LDLLFileName: string;
  LCount: Integer;
begin
  LDLLFileName := ExtractFilePath(ParamStr(0)) + FDLLName;
  Check(FileExists(LDLLFileName), 'Cannot find ' + FDLLName);
  FDLLProject := LoadModuleTests(LDLLFileName);
  Check(Assigned(FDLLProject), 'FDLLProject not assigned');
  Check(FDLLProject.DisplayedName = FDLLFullPathAndName,
    'FDLLProject incorrect. Was ' + FDLLProject.DisplayedName + ' but should be ' + FDLLFullPathAndName);
  FDLLProject.DisplayedName := FDLLName; // Shorten it for ease
  TestFramework.RegisterProject(FDLLProject);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FDLLName,
    'Returned project name should be ' + FDLLName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.IsTestMethod = False, 'Project should not show as TestMethod');;
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 1,
    'EnabledTestCases should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsSameSingleAutoRegisteredDLL;
var
  LCount: Integer;
begin
  Check(FileExists(FDLLFullPathAndName), 'Cannot find ' + FDLLFullPathAndName);
  RegisterModuleTests(FDLLFullPathAndName);
  FDLLProject := TestFramework.Projects;
  Check(Assigned(FDLLProject), 'FDLLProject not assigned');
  Check(FDLLProject.DisplayedName = FDLLName,
    'FDLLProject incorrect. Was ' + FDLLProject.DisplayedName + ' but should be ' + FDLLName);

  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FDLLName,
    'Returned FTestFrameworkProxy name should be ' + FDLLName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.IsTestMethod = False, 'Project should not show as TestMethod');;
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 1,
    'EnabledTestCases should be 1 but was ' + IntToStr(LCount));
end;

procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsOneRegisteredProjectAndOneRegisteredDLL;
var
  LCount: Integer;
  LPM: IProjectManager;
  LExecControl: ITestExecControl;
  LTest: ITest;
begin
  FProject1 := nil;  // Not required for this test
  TestFramework.ProjectRegisterTest('', TSimpleTestCase2.Suite);

  Check(FileExists(FDLLFullPathAndName), 'Cannot find ' + FDLLFullPathAndName);
  RegisterModuleTests(FDLLFullPathAndName);
  LPM := TestFramework.Projects.Manager as IProjectManager;

  // Moved here for quick check
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FExeName,
    'Returned project name should be ' + FExeName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = '',
    'Parent path should be empty but is <' + FTestFrameworkProxy.ParentPath + '>');

  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 2,
    'List of projects added should be 2 but was ' + IntToStr(LCount));
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 3,
    'EnabledTestCases should be 3 but was ' + IntToStr(LCount));
  LExecControl := LPM.Projects.ExecutionControl;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  LPM.Projects.Run(LExecControl);
  EarlyExitCheck(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));

  // Early Exit Check Failed
  LCount := LPM.Count;
  Check(LCount = 2,
    'Count of Projects should be 2 but was ' + IntToStr(LCount));

  FProject1 := LPM.Project[0];
  Check(Assigned(FProject1), 'Project at idx=0 ' + DefaultProject + 'should not be nil');
  LCount := FProject1.CountEnabledTests;
  Check(LCount = 2,
    'Count of enabled tests in Project1 should be 2 but was ' + IntToStr(LCount));

  FDLLProject := LPM.Project[1];
  Check(Assigned(FDLLProject), 'Project at idx=1 DLL Project should not be nil');
  LCount := FDLLProject.CountEnabledTests;
  Check(LCount = 1,
    'Count of enabled tests in DLL Project should be 1 but was ' + IntToStr(LCount));
  LCount := TestFramework.Projects.CountEnabledTests;
  Check(LCount = 3,
    'Count of enabled tests should be 3 but was ' + IntToStr(LCount));

  LTest := TestFramework.Projects.FindFirstTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = DefaultProject,
  'Test name should be ' + DefaultProject + ' but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'TSimpleTestCase2',
  'Test name should be TSimpleTestCase2 but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'FirstTestPasses',
  'Test name should be FirstTestPasses but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'SecondTestPasses',
  'Test name should be SecondTestPasses but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'SingleTestLibW32.dtl',
  'Test name should be SingleTestLibW32.dtl but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'TSingleDLLTest',
  'Test name should be TSingleDLLTest .dtl but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'SingleTestDLL',
  'Test name should be SingleTestDLL but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(LTest = nil, 'After last fetch result should be nil');

{
//  Moved to top for early exit check
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FExeName,
    'Returned project name should be ' + FExeName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = '',
    'Parent path should be empty but is <' + FTestFrameworkProxy.ParentPath + '>');

  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 3,
    'EnabledTestCases should be 3 but was ' + IntToStr(LCount));

  LExecControl := LPM.Projects.ExecutionControl;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  LPM.Projects.Run(LExecControl);
  Check(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));
  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 2,
    'List of projects added should be 2 but was ' + IntToStr(LCount));
}
end;

// Reverse the sequence of registering to see if there is any order dependency
procedure TTestProxyFrameworkRegistersProjects.VerifyRegisteredProxyReturnsOneRegisteredDLLAndOneRegisteredProject;
var
  LCount: Integer;
  LPM: IProjectManager;
  LExecControl: ITestExecControl;
  LTest: ITest;
begin
  FProject1 := nil;
  Check(FileExists(FDLLFullPathAndName), 'Cannot find ' + FDLLFullPathAndName);
  RegisterModuleTests(FDLLFullPathAndName);
  TestFramework.ProjectRegisterTest('', TSimpleTestCase2.Suite);

  LPM := TestFramework.Projects.Manager as IProjectManager;
  LCount := LPM.Count;
  Check(LCount = 2,
    'Count of projects should be 2 but was ' + IntToStr(LCount));

  FDLLProject := LPM.Project[0];
  Check(Assigned(FDLLProject), 'DLL Project at idx=0 should not be nil');
  LCount := FDLLProject.CountEnabledTests;
  Check(LCount = 1,
    'Count of enabled tests in DLL Project should be 1 but was ' + IntToStr(LCount));

  FProject1 := LPM.Project[1];
  Check(Assigned(FProject1), 'Project at idx=1 should not be nil');
  LCount := FProject1.CountEnabledTests;
  Check(LCount = 2,
    'Count of enabled tests in 2nd Project should be 2 but was ' + IntToStr(LCount));
  LCount := TestFramework.Projects.CountEnabledTests;
  Check(LCount = 3,
    'Count of enabled tests should be 3 but was ' + IntToStr(LCount));

  LTest := TestFramework.Projects.FindFirstTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'SingleTestLibW32.dtl',
  'Test name should be SingleTestLibW32.dtl but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'TSingleDLLTest',
  'Test name should be TSingleDLLTest .dtl but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'DLL Must exist');
  Check(LTest.DisplayedName = 'SingleTestDLL',
  'Test name should be SingleTestDLL but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), 'Defaut Project must exist');
  Check(LTest.DisplayedName = DefaultProject,
  'Test name should be ' + DefaultProject + ' but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'TSimpleTestCase2',
  'Test name should be TSimpleTestCase2 but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'FirstTestPasses',
  'Test name should be FirstTestPasses but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(Assigned(LTest), DefaultProject + ' must exist');
  Check(LTest.DisplayedName = 'SecondTestPasses',
  'Test name should be SecondTestPasses but was ' + LTest.DisplayedName);

  LTest := TestFramework.Projects.FindNextTest;
  Check(LTest = nil, 'After last fetch result should be nil');

  // Setup conditions are correct, now test the behavior
  // Build the tree of projects which is passed to the GUI to make visual.
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FExeName,
    'Returned project name should be ' + FExeName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = '',
    'Parent path should be empty but is <' + FTestFrameworkProxy.ParentPath + '>');
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 3,
    'EnabledTestCases should be 3 but was ' + IntToStr(LCount));

  LExecControl := LPM.Projects.ExecutionControl;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  LPM.Projects.Run(LExecControl);
  Check(LExecControl.ExecutionCount = 3,
    'Execution count should be 3 but was ' + IntToStr(LExecControl.ExecutionCount));

  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 2,
    'List of returned projects should be 2 but was ' + IntToStr(LCount));
end;
{$endif}

  {$ifdef fastmm}
    {$ifndef clr}
      {$ifndef VER130}
        {$IFNDEF VER140}
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
    'SequenceNo should be 2 in ThisFailsDueSetUpLeak but was ' + IntToStr(FSequenceNo));
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
    'SequenceNo should be 5 in ThisFailsDueTearDownLeak but was ' + IntToStr(FSequenceNo));
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
  Check(Assigned(FRunObject), 'Never fails');
end;

procedure TLeaksAndPasses.ThisLeaksOnRunB;
begin
  Check(FSequenceNo = 8,
    'SequenceNo should be 8 in ThisLeaksOnRun but was ' + IntToStr(FSequenceNo));
  FRunObjectB := TObject.Create;
  Check(Assigned(FRunObject), 'Never fails');
end;

procedure TLeaksAndPasses.ThisPassesAfterSucessiveLeaks;
begin
  Check(FSequenceNo = 9,
    'SequenceNo should be 9 in ThisPassesAfterRun but was ' + IntToStr(FSequenceNo));
  Check(Assigned(FRunObjectA), 'Never fails');
  Check(Assigned(FRunObjectB), 'Never fails');
end;


{ TTestMultiLeakHandling }

procedure TTestMultiLeakHandling.SetUpOnce;
begin
  FXMLFile := LocalAppDataPath + 'XXX' + ChangeFileExt(ExtractFileName(ParamStr(0)), '.xml');
  FExeName := ExtractFileName(ParamStr(0));
end;

procedure TTestMultiLeakHandling.SetUp;
begin
  FProject1 := nil;
  FProject1 := TestFramework.TTestProject.Create;
  FProject1.ParentPath := FExeName;
  FTestResult := nil;
  try
    if FileExists(FXMLFile) then
      DeleteFile(FXMLFile);
  except
  end;
end;

procedure TTestMultiLeakHandling.TearDown;
begin
  if Assigned(FProject1) then
    FProject1.ReleaseProxys;
  FProject1 := nil;
  if Assigned(FTestFrameworkProxy) then
    FTestFrameworkProxy.ReleaseTests;
  FTestFrameworkProxy := nil;
  FTestResult := nil;
  TestFramework.UnRegisterProjectManager;
  try
    if FileExists(FXMLFile) then
      DeleteFile(FXMLFile);
  except
  end;
end;

procedure TTestMultiLeakHandling.TearDownOnce;
begin
  FExeName := '';
  FXMLFile := '';
end;

procedure TTestMultiLeakHandling.ValidateMultiLeakHandling;
var
  LCount: Integer;
  LPM: IProjectManager;
  LExecControl: ITestExecControl;
begin
  FProject1 := nil;
  TestFramework.ProjectRegisterTest('', TLeaksAndPasses.Suite);
  LPM := TestFramework.Projects.Manager as IProjectManager;
  LCount := LPM.Count;
  Check(LCount = 1,
    'Count of Projects tests should be 1 but was ' + IntToStr(LCount));
  FProject1 := LPM.Project[0];
  Check(Assigned(FProject1), 'Project at idx=0 ' + DefaultProject + 'should not be nil');
  LCount := FProject1.CountEnabledTests;
  Check(LCount = 9,
    'Count of enabled tests in project should be 9 but was ' + IntToStr(LCount));

  // Setup conditions are correct, now test the behavior
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy <> nil, 'RegisteredTests should not be nil.');
  Check(FTestFrameworkProxy.Name = FExeName,
    'Returned project name should be ' + FExeName + ' but is <' + FTestFrameworkProxy.Name + '>');
  Check(FTestFrameworkProxy.ParentPath = '',
    'Parent path should be empty but is <' + FTestFrameworkProxy.ParentPath + '>');
  LCount := FTestFrameworkProxy.Tests.Count;
  Check(LCount = 1,
    'List of added projects should be 1 but was ' + IntToStr(LCount));
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 9,
    'EnabledTestCases should be 9 but was ' + IntToStr(LCount));

  LExecControl := TestFramework.TestExecControl;
  LExecControl.InhibitStackTrace := FExecControl.FailsOnMemoryLeak;
  FProject1.Run(LExecControl);
  Check(LExecControl.ExecutionCount = 9,
    'Execution count should be 9 but was ' + IntToStr(LExecControl.ExecutionCount));
end;

procedure TTestMultiLeakHandling.ValidateXMLEffectOnMultiLeakHanding;
var
  LCount: Integer;
  LPM: IProjectManager;
begin
  FProject1 := nil;
  TestFramework.ProjectRegisterTest('', TLeaksAndPasses.Suite);
  LPM := TestFramework.Projects.Manager as IProjectManager;
  FProject1 := LPM.Project[0];
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  FTestResult := GetTestResult;
  FTestResult.AddListener(TXMLListener.Create(FXMLFile));
  TestFrameWork.RegisteredTests.FailsOnMemoryLeak := True;
  LCount := FTestFrameworkProxy.CountEnabledTestCases;
  Check(LCount = 9,
    'EnabledTestCases should be 9 but was ' + IntToStr(LCount));

  // Setup conditions are correct, now test the behavior
  try
    FTestFrameworkProxy.Run(FTestResult);
  finally
    FTestResult.ReleaseListeners; // should allow xml report to be written
  end;

  Check(FTestResult.RunCount = 9,
    'TestResult RunCount should be 9 but was ' + IntToStr(FTestResult.RunCount));
  Check(FTestResult.FailureCount = 5,
    'TestResult Failure count should be 5 but was ' + IntToStr(FTestResult.FailureCount));
  Check(FTestResult.ErrorCount = 0,
    'TestResult Error count should be 0 but was ' + IntToStr(FTestResult.ErrorCount));

  Check(FileExists(FXMLFile), 'XML File was not created');
end;
        {$ENDIF}
      {$endif}
    {$endif}
  {$endif}

{ TTestBreakOnFailure }

procedure TTestBreakOnFailure.SetUp;
begin
  FAnExecControl := nil;
  if Assigned(FTestProject) then
    FTestProject.ReleaseProxys;
  FTestProject := nil;

  if Assigned(FTestFrameworkProxy) then
    FTestFrameworkProxy.ReleaseTests;
  FTestFrameworkProxy := nil;
  TestFramework.UnRegisterProjectManager;
end;

procedure TTestBreakOnFailure.TearDown;
begin
  SetUp;
end;

procedure TTestBreakOnFailure.VerifyTestsBreakOnFailure;
var
  LTestResult: ITestResult;
begin
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy = nil, 'Should be nil until project registered.');
  TestFramework.RegisterTest(TMiddleOfThreeTestsFail.Suite);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(Assigned(FTestFrameworkProxy), 'Project tree should now be registered');
  LTestResult := GetTestResult;
  FAnExecControl := TestFramework.Projects.ExecutionControl;
  // Prevent an apparent memory leak caused by stack traceing on errors and failures
  FAnExecControl.InhibitStackTrace := True;
  FTestFrameworkProxy.Run(LTestResult);
  Check(FAnExecControl.ExecutionCount = 3, 'Should be 3 tests executed');
  Check(FAnExecControl.FailureCount = 1, 'Should be 1 test failure');
  Check(FAnExecControl.WarningCount = 0, 'Should be 0 test warnings');

  LTestResult.BreakOnFailures := True;
  FTestFrameworkProxy.Run(LTestResult);
  Check(FAnExecControl.ExecutionCount = 2, 'Should be 2 tests executed but have ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.FailureCount = 1, 'Should be 1 test failure');
  Check(FAnExecControl.ErrorCount = 0, 'Should be 0 test errors');
  Check(FAnExecControl.WarningCount = 0, 'Should be 0 test warnings');
end;

procedure TTestBreakOnFailure.VerifyTestsBreakOnException;
var
  LTestResult: ITestResult;
begin
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(FTestFrameworkProxy = nil, 'Should be nil until project registered.');
  TestFramework.RegisterTest(TMiddleOfThreeTestsExcept.Suite);
  FTestFrameworkProxy := TestFrameworkProxy.RegisteredTests;
  Check(Assigned(FTestFrameworkProxy), 'Project tree should now be registered');
  LTestResult := GetTestResult;
  FAnExecControl := TestFramework.Projects.ExecutionControl;
  // Prevent an apparent memory leak caused by stack traceing on errors and failures
  FAnExecControl.InhibitStackTrace := True;
  FTestFrameworkProxy.Run(LTestResult);
  Check(FAnExecControl.ExecutionCount = 3, 'Should be 3 tests executed');
  Check(FAnExecControl.ErrorCount = 1, 'Should be 1 test error');
  Check(FAnExecControl.WarningCount = 0, 'Should be 0 test warnings');

  LTestResult.BreakOnFailures := True;
  FTestFrameworkProxy.Run(LTestResult);
  Check(FAnExecControl.ExecutionCount = 2, 'Should be 2 tests executed but have ' + IntToStr(FAnExecControl.ExecutionCount));
  Check(FAnExecControl.FailureCount = 0, 'Should be 0 test failures but have ' + IntToStr(FAnExecControl.FailureCount));
  Check(FAnExecControl.ErrorCount = 1, 'Should be 1 test failure');
  Check(FAnExecControl.WarningCount = 0, 'Should be 0 test warnings');
end;

initialization
  {$ifdef selftest} RefTestFramework. {$endif}
  RegisterTests('Validate_TestFrameworkProxy', [TTestProxyFrameworkRegistersProjects.Suite
                                                {$IFNDEF VER130}
                                                  {$IFNDEF VER140}
                                                    {$IFDEF FASTMM}
                                                      ,TTestMultiLeakHandling.Suite
                                                    {$ENDIF}
                                                  {$ENDIF}
                                                {$ENDIF}
                                               ]);
  {$ifdef selftest} RefTestFramework. {$endif}
  RegisterTest('Validate_TestFrameworkProxy', TTestBreakOnFailure.Suite);
end.
