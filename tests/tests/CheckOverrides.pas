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

unit CheckOverrides;

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
  {$IFDEF SELFTEST}
    RefTestFramework;
  {$ELSE}
    TestFramework;
  {$ENDIF}

type

  TTestGUICommandsFailureOnNoCheckCalled = class(TTestCase)
    published
      procedure TestCanShowFailureWhenTestFails;
      procedure TestCanShowWarningWhenGUICommandGetsOverridden;
      procedure TestCanShowOverRideWhenPassingTestContainsOverrideOfGUICommand;
      procedure TestCanShowPassWhenGUICommandToPassingTestNotOverridden;
      procedure TestCanShowException;
      procedure TestCanShowAnotherException;
  end;

{$IFDEF FASTMM}
  TTestMemFailPropReports = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowFailureWhenGUICommandsFailOnMemLeak;
  end;

  TTestMemFailPropORideReports = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowWarningWhenGUICommandsFailOnMemLeakGetsOverridden;
  end;

  TTestMemPropORideReports = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowOverRideWhenPassingTestContainsOverRideOfGUICommand;
  end;

  TTestAllowedLeakReports = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowAllowedLeakSizePresent;
      procedure TestCanShowsAllowedLeakOverRideOfGUICommand;
      procedure TestIsClean;
  end;

  TTestMemPropPassReports = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowPassOnNonLeakyTestAndGUICommandNotOverridden;
  end;

  TTestSetUpLeakReportEnable = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUp; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowPassOnIgnoredSetUpLeakGUINotOverridden;
    end;

  TTestSetUpLeakReportDisable = class(TTestCase)
    private
      ObjA: TObject;
    protected
      procedure SetUp; override;
      procedure TearDownOnce; override;
    published
      procedure TestCanShowPassOnIgnoredSetUpLeakGUIOverridden;
    end;
{$ENDIF}

  TTestMissingCheckWarningReport = class(TTestCase)
    published
      procedure TestPasses;
      procedure TestGensWarningDueDoesNotCallCheck;
      procedure TestGensWarningDueDoesNotCallCheckUserMessage1;
      procedure TestGensWarningDueDoesNotCallCheckUserMessage2;
      procedure TestGensWarningDueDoesNotCallCheckNoUserMessage;
      procedure PassingTestWithForceFailCommandIdentified;
      procedure TestGensWarningDueOverrideOfDoesNotCallCheck;
    end;

  TTestLeakTestScenarioWarningReport = class(TTestCase)
  private
    FObject1: TObject;
    FObject2: TObject;
    FObject3: TObject;
    FObject4: TObject;
    FObject5: TObject;
    protected
      procedure SetUpOnce; override;
      procedure TearDownOnce; override;
    published
      procedure TestPasses;
      procedure TestGensWarningDueMemLeak;
      procedure TestGensWarningDueMemLeakUserMessage1;
      procedure TestGensWarningDueMemLeakUserMessage2;
      procedure TestGensWarningDueMemLeakNoUserMessage;
      procedure PassingNoLeakTestWithForceFailIdentified;
      procedure TestGensWarningDueOverridedOfMemLeak;
  end;

implementation
uses
  SysUtils;

{ TTestCheckPropReports }

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowWarningWhenGUICommandGetsOverridden;
begin
  FailsOnNoChecksExecuted := False;
end;

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowPassWhenGUICommandToPassingTestNotOverridden;
begin
  Check(True);
end;

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowOverRideWhenPassingTestContainsOverrideOfGUICommand;
begin
  Check(True);
  FailsOnNoChecksExecuted := False;
end;

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowException;
begin
  raise eDivByZero.Create('Deliberate divide by zero exception');
end;

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowAnotherException;
begin
  raise EOverflow.Create('Deliberate Overflow exception');
end;

procedure TTestGUICommandsFailureOnNoCheckCalled.TestCanShowFailureWhenTestFails;
begin
  // The following ensures there is code generated so the test is not empty
  FailsOnNoChecksExecuted := FailsOnNoChecksExecuted;
end;

{ TTestMemPropReports }

{$IFDEF FASTMM}
procedure TTestMemFailPropReports.SetUpOnce;
begin
  ObjA := nil;
end;

procedure TTestMemFailPropReports.TestCanShowFailureWhenGUICommandsFailOnMemLeak;
begin
  ObjA := TObject.Create;
  Check(Assigned(ObjA), 'Crikey, if this fails were in deep trouble');
end;

procedure TTestMemFailPropReports.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

{ TTestMemFailPropORideReports }

procedure TTestMemFailPropORideReports.SetUpOnce;
begin
  ObjA := nil;
end;

procedure TTestMemFailPropORideReports.TestCanShowWarningWhenGUICommandsFailOnMemLeakGetsOverridden;
begin
  ObjA := TObject.Create;
  Check(Assigned(ObjA), 'Crikey, if this fails were in deep trouble');
  FailsOnMemoryLeak := False;
end;

procedure TTestMemFailPropORideReports.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

procedure TTestMemPropORideReports.SetUpOnce;
begin
  ObjA := nil;
end;

procedure TTestMemPropORideReports.TestCanShowOverRideWhenPassingTestContainsOverRideOfGUICommand;
begin
  Check(not Assigned(ObjA), 'Crikey, if this fails were in deep trouble');
  FailsOnMemoryLeak := False;
end;

procedure TTestMemPropORideReports.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

procedure TTestMemPropPassReports.SetUpOnce;
begin
  ObjA := nil;
end;

procedure TTestMemPropPassReports.TestCanShowPassOnNonLeakyTestAndGUICommandNotOverridden;
begin
  Check(not Assigned(ObjA), 'Crikey, if this fails were in deep trouble');
end;

procedure TTestMemPropPassReports.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

{ TTestAllowedLeakReports }

procedure TTestAllowedLeakReports.SetUpOnce;
begin
  ObjA := nil;
end;

procedure TTestAllowedLeakReports.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

procedure TTestAllowedLeakReports.TestCanShowAllowedLeakSizePresent;
begin
  Check(not Assigned(ObjA), 'ObjA must be nil');
  AllowedMemoryLeakSize := 16;
end;

procedure TTestAllowedLeakReports.TestCanShowsAllowedLeakOverRideOfGUICommand;
begin
  Check(not Assigned(ObjA), 'ObjA must be nil');
  ObjA := TObject.Create;
  AllowedMemoryLeakSize := 16;
end;

procedure TTestAllowedLeakReports.TestIsClean;
begin
  Check(True);
end;

{ TTestSetUpLeakReport }

procedure TTestSetUpLeakReportEnable.SetUp;
begin
  ObjA := TObject.Create;
end;

procedure TTestSetUpLeakReportEnable.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

procedure TTestSetUpLeakReportEnable.TestCanShowPassOnIgnoredSetUpLeakGUINotOverridden;
begin
  IgnoresMemoryLeakInSetUpTearDown := True;
  Check(Assigned(ObjA), 'Should be assigned');
end;


{ TTestSetUpLeakReportDisable }

procedure TTestSetUpLeakReportDisable.SetUp;
begin
  ObjA := TObject.Create;
end;

procedure TTestSetUpLeakReportDisable.TearDownOnce;
begin
  FreeAndNil(ObjA);
end;

procedure TTestSetUpLeakReportDisable.TestCanShowPassOnIgnoredSetUpLeakGUIOverridden;
begin
  IgnoresMemoryLeakInSetUpTearDown := False;
  Check(Assigned(ObjA), 'Should be assigned');
end;

{$ENDIF}


{ TTestMissingCheckWarningReport }

procedure TTestMissingCheckWarningReport.PassingTestWithForceFailCommandIdentified;
begin
  FailsOnNoChecksExecuted := True;
  TestPasses;
end;

procedure TTestMissingCheckWarningReport.TestGensWarningDueDoesNotCallCheck;
begin
  ErrorMessage := ErrorMessage; // Just so the test proc isn't empty.
end;

procedure TTestMissingCheckWarningReport.TestGensWarningDueDoesNotCallCheckUserMessage1;
begin
  ErrorMessage := 'User message1';
end;

procedure TTestMissingCheckWarningReport.TestGensWarningDueDoesNotCallCheckUserMessage2;
begin
  ErrorMessage := 'User message2';
end;

procedure TTestMissingCheckWarningReport.TestGensWarningDueDoesNotCallCheckNoUserMessage;
begin
  ErrorMessage := ErrorMessage; // Just so the test proc isn't empty.
end;

procedure TTestMissingCheckWarningReport.TestPasses;
begin
  Check(True);
end;

procedure TTestMissingCheckWarningReport.TestGensWarningDueOverrideOfDoesNotCallCheck;
begin
  TestGensWarningDueDoesNotCallCheck;
  FailsOnNoChecksExecuted := False;
end;

{ TTestLeakTestScenarioWarningReport }

procedure TTestLeakTestScenarioWarningReport.SetUpOnce;
begin
  inherited;
  FObject1 := nil;
  FObject2 := nil;
  FObject3 := nil;
  FObject4 := nil;
  FObject5 := nil;
end;

procedure TTestLeakTestScenarioWarningReport.TearDownOnce;
begin
  FreeAndNil(FObject1);
  FreeAndNil(FObject2);
  FreeAndNil(FObject3);
  FreeAndNil(FObject4);
  FreeAndNil(FObject5);
  inherited;
end;

procedure TTestLeakTestScenarioWarningReport.TestPasses;
begin
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.TestGensWarningDueMemLeak;
begin
  FObject1 := TObject.Create;
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.TestGensWarningDueMemLeakUserMessage1;
begin
  FObject2 := TObject.Create;
  ErrorMessage := 'User Leak message1';
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.TestGensWarningDueMemLeakUserMessage2;
begin
  FObject3 := TObject.Create;
  ErrorMessage := 'User Leak message2';
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.TestGensWarningDueMemLeakNoUserMessage;
begin
  FObject4 := TObject.Create;
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.PassingNoLeakTestWithForceFailIdentified;
begin
  FailsOnMemoryLeak := True;
  Check(True);
end;

procedure TTestLeakTestScenarioWarningReport.TestGensWarningDueOverridedOfMemLeak;
begin
  FObject5 := TObject.Create;
  FailsOnMemoryLeak := False;
  Check(True);
end;

initialization
  {$IFDEF SELFTEST}
    RefTestFramework.Registertest('NoCheckCalledOverrideTests', TTestGUICommandsFailureOnNoCheckCalled.Suite);
    {$IFDEF FASTMM}
      RefTestFramework.RegisterTests('MemoryLeakOverrideTests', [TTestMemFailPropReports.Suite,
                                                                 TTestMemFailPropORideReports.Suite,
                                                                 TTestMemPropORideReports.Suite,
                                                                 TTestMemPropPassReports.Suite,
                                                                 TTestAllowedLeakReports.Suite,
                                                                 TTestSetUpLeakReportEnable.Suite,
                                                                 TTestSetUpLeakReportDisable.Suite
                                                                 ]);
    {$ENDIF}
  {$ELSE}
    RegisterTest(TTestGUICommandsFailureOnNoCheckCalled.Suite);
    {$IFDEF FASTMM}
      RegisterTests('MemoryLeakOverrideTests', [TTestMemFailPropReports.Suite,
                                                TTestMemFailPropORideReports.Suite,
                                                TTestMemPropORideReports.Suite,
                                                TTestMemPropPassReports.Suite,
                                                TTestAllowedLeakReports.Suite,
                                                TTestSetUpLeakReportEnable.Suite,
                                                TTestSetUpLeakReportDisable.Suite
                                                ]);
    {$ENDIF}
    RegisterTests('Warnings Demo', [ TTestMissingCheckWarningReport.Suite
                                    ,TTestLeakTestScenarioWarningReport.Suite]);
  {$ENDIF}
end.
