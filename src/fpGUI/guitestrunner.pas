unit GuiTestRunner;

{$mode objfpc}{$H+}

{.$define gDEBUG}
{$IFDEF gDEBUG}
  {$ASSERTIONS ON}
{$ENDIF}

{ if enabled, test results will be written to a XML file }
{.$DEFINE XMLLISTENER}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_label, fpg_menu,
  fpg_grid, fpg_tree, fpg_imagelist, fpg_command_intf,
  fpg_panel, fpg_button, fpg_splitter, fpg_gauge, fpg_memo, fpg_basegrid,
  fpg_scrollbar, CustomGauge,
  TestFrameworkProxyIfaces;

type
  // forward declarations
  TExitCommand = class;

  { Function type used by the TDUnitDialog.ApplyToTests method
     item:   The ITest instance on which to act
     return: true if processing should continue, false otherwise }
  TTestFunc = function (item: ITestProxy): Boolean of object;


  TGUITestRunner = class(TfpgForm, ITestListener, ITestListenerX)
  private
    {@VFD_HEAD_BEGIN: GUITestRunner}
    MainMenu: TfpgMenuBar;
    Toolbar: TfpgBevel;
    btnSelectAll: TfpgButton;
    btnSelectNone: TfpgButton;
    Bevel1: TfpgBevel;
    btnSelectFailed: TfpgButton;
    btnSelectCurrent: TfpgButton;
    btnDeselectCurrent: TfpgButton;
    Bevel2: TfpgBevel;
    btnRunAllSelected: TfpgButton;
    btnRunCurrentSelection: TfpgButton;
    btnStopTests: TfpgButton;
    Bevel3: TfpgBevel;
    Button9: TfpgButton;
    Button10: TfpgButton;
    Button11: TfpgButton;
    Bevel4: TfpgBevel;
    Button12: TfpgButton;
    Button13: TfpgButton;
    Button14: TfpgButton;
    Button15: TfpgButton;
    Label1: TfpgLabel;
    ClientArea: TfpgBevel;
    bvlTreeAndProgress: TfpgBevel;
    mnuFile: TfpgPopupMenu;
    mnuTestTree: TfpgPopupMenu;
    mnuOptions: TfpgPopupMenu;
    mnuActions: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    Bevel5: TfpgBevel;
    TestTree: TfpgTreeView;
    ProgressBar: TfpgGauge;
    ScoreBar: TGUIRunnerGauge;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    ResultsView: TfpgStringGrid;
    Splitter2: TfpgSplitter;
    meErrorMessage: TfpgMemo;
    FailureGrid: TfpgStringGrid;
    {@VFD_HEAD_END: GUITestRunner}
    miAutoSaveConfiguration: TfpgMenuItem;
    miErrorBoxVisible: TfpgMenuItem;
    miAutoChangeFocus: TfpgMenuItem;
    miHideTestNodesOnOpen: TfpgMenuItem;
    miShowTestedNode: TfpgMenuItem;
    miBreakOnFailure: TfpgMenuItem;
    miShowTestCasesWithRuntimeProperties: TfpgMenuItem;
    miShowOverriddenFailures: TfpgMenuItem;
    miShowExitedEarly: TfpgMenuItem;
    miFailTestIfNoChecks: TfpgMenuItem;
    miEnableWarnings: TfpgMenuItem;
    miInhibitSummaryLevelChecks: TfpgMenuItem;
    FSuite:         ITestProxy;
    FTestResult:    ITestResult;
    FRunning:       Boolean;
    FTests:         TInterfaceList;
    FSelectedTests: TInterfaceList;
    FTotalTime:     Extended;
    FNoChecksStr:   string;
    FUpdateTimer:   TfpgTimer;
    FTimerExpired:  Boolean;
    TotalTestsCount: Integer;
    FMemLeakStr:    string;
    FMemBytesStr:   string;
    FBytes:         string;
    FIgnoreLeakStr: string;
    FMemLeakDetectedPtyOverridden: Boolean;
    FIgnoreSetUpTearDownLeakPtyOverridden: Boolean;
    FNoCheckExecutedPtyOverridden: Boolean;
    FPopupY: Integer;
    FPopupX: Integer;
    FHoldOptions: boolean;
    FTestFailed: Boolean;
    FImageList: TfpgImageList;
    FStateImageList: TfpgImageList;

    SelectAllCommand: ICommand;
    HideTestNodesCommand: ICommand;
    ExpandAllNodesCommand: ICommand;
    miTreeSelectAll: TfpgMenuItem;
    miTreeDeselectAll: TfpgMenuItem;
    miTreeSelectFailed: TfpgMenuItem;
    miRunAllSelected: TfpgMenuItem;
    miRunCurrentSelection: TfpgMenuItem;

    { implement the IStatusListeners interface }
    procedure Status(const ATest: ITestProxy; AMessage: string);

    { implement the ITestListener interface }
    procedure AddSuccess(ATest: ITestProxy);
    procedure AddError(AError: TTestFailure);
    procedure AddFailure(AFailure: TTestFailure);
    procedure AddWarning(AWarning: TTestFailure);
    procedure TestingStarts;
    procedure StartTest(Test: ITestProxy);
    procedure EndTest(Test: ITestProxy);
    procedure TestingEnds(TestResult: ITestResult);
    function  ShouldRunTest(const ATest :ITestProxy):Boolean;

    { implement the ITestListenerX interface }
    procedure StartSuite(Suite: ITestProxy);
    procedure EndSuite(Suite: ITestProxy);

    procedure CommandHandler(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ProcessClickOnStateIcon(Sender: TObject; ANode: TfpgTreeNode);
    procedure ProcessKeyPressOnTreeview(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure DrawGridCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure OnTimerFired(Sender: TObject);
    procedure TestNodeSelected(Sender: TObject);
    procedure SetSuite(const AValue: ITestProxy);
    function get_TestResult: ITestResult;
    procedure set_TestResult(const AValue: ITestResult);
    procedure LoadSuiteConfiguration;
    procedure EnableUI(AEnable: Boolean);
    function  IniFileName: TfpgString;
    procedure InitCommands;
    function  EnableTest(Test: ITestProxy): boolean;
    function  DisableTest(Test: ITestProxy): boolean;
    procedure ApplyToTests(root: TfpgTreeNode; const func: TTestFunc);
    function  AddFailureItem(Failure: TTestFailure): integer;
    procedure UpdateStatus(const FullUpdate: Boolean);
    procedure FillTestTree(ARootNode: TfpgTreeNode; ATest: ITestProxy); overload;
    procedure FillTestTree(ATest: ITestProxy); overload;
    procedure SetUp;
    function  NodeToTest(ANode: TfpgTreeNode): ITestProxy;
    function  TestToNode(Test: ITestProxy): TfpgTreeNode;
    function  SelectedTest: ITestProxy;
    procedure ListSelectedTests;
    procedure SwitchNodeState(ANode: TfpgTreeNode);
    procedure UpdateTestTreeState;
    procedure SetNodeState(ANode: TfpgTreeNode; AEnabled: boolean);
    procedure UpdateNodeImage(ANode: TfpgTreeNode);
    procedure UpdateNodeState(ANode: TfpgTreeNode);
    procedure RefreshTestCount;
    procedure AutoSaveConfiguration;
    procedure SaveConfiguration;
    procedure LoadConfiguration;
    procedure SaveFormPlacement;
    procedure LoadFormPlacement;
    function  NodeIsGrandparent(ANode: TfpgTreeNode): boolean;
    procedure CollapseNonGrandparentNodes(RootNode: TfpgTreeNode);
    function HasParentNode(ANode: TfpgTreeNode): Boolean;
    procedure SetupGUINodes;
    procedure MakeNodeVisible(Node: TfpgTreeNode);
    procedure SetTreeNodeImage(Node: TfpgTreeNode; imgIndex: Integer);
    procedure ClearStatusMessage;
    procedure RunTheTest(ATest: ITestProxy);
    procedure HoldOptions(const AValue: boolean);
    procedure ClearResult;
    procedure DisplayFailureMessage(ARow: integer);
    procedure ClearFailureMessage;
    procedure ResetProgress;
    procedure SetProgressBarColor(const AColor: TfpgColor);
    procedure AutoSaveActionExecute(Sender: TObject);
    procedure HideTestNodesOnOpenClicked(Sender: TObject);
    procedure ShowTestedNodeClicked(Sender: TObject);
    procedure EnableWarningsActionExecute(Sender: TObject);
    procedure FailureGridRowChanged(Sender: TObject; ARow: Integer);
    procedure ClearFailureGrid;
    procedure miHelpAboutFPGui(Sender: TObject);
    procedure miHelpAboutFPTest(Sender: TObject);
  protected
    procedure InitTree; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  published
    { The Test Suite to be run in this runner }
    property Suite: ITestProxy read FSuite write SetSuite;
    { The result of the last Test run }
    property TestResult: ITestResult read get_TestResult write set_TestResult;
  end;


  TFailureDataObject = class(TObject)
  public
    TreeNode: TfpgTreeNode;
    ImageIndex: integer;
    Caption: TfpgString;
    ThrownExceptionName: string;
    FilteredErrorMessage: string;
    LocationInfo: string;
    FullTestPath: string;
    ErrorMessage: string;
  end;


  TBaseCommand = class(TInterfacedObject, ICommand)
  protected
    FForm: TGUITestRunner;
  public
    constructor Create(AForm: TGUITestRunner); reintroduce;
    procedure Execute; virtual; abstract;
  end;


  TExitCommand = class(TBaseCommand)
  public
    procedure   Execute; override;
  end;


  TSelectAllCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TSelectCurrentCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TDeselectCurrentCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TRunAllSelectedCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TRunCurrentSelectionCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TDeselectAllCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TSelectFailedCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TStopTestsCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  THideTestNodesOnOpenCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TExpandAllNodesCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


procedure RunRegisteredTests;


implementation

uses
  fpg_utils,
  fpg_iniutils,
  fpg_dialogs,
  dbugintf,
  TestFrameworkProxy,
  {$IFDEF XMLLISTENER}
  XMLListener,
  {$ENDIF}
  TimeManager,
  formimages;

const
  TEST_INI_FILE = 'fptest.ini';

  { Section of the dunit.ini file where GUI information will be stored }
  cnConfigIniSection = 'GUITestRunner Config';

  { Color constants for the progress bar and failure details panel }
  clOK        = clGreen;
  clFAILURE   = clFuchsia;
  clERROR     = clRed;

  { Indexes of the color images used in the Test tree and failure list }
  imgNONE      = 0;
  imgRUNNING   = 1;
  imgRUN       = 2;
  imgOVERRIDE  = 3;
  imgWARNFAIL  = 4;
  imgFAILED    = 5;
  imgERROR     = 6;
  imgWARNING   = 7;
  imgEARLYEXIT = 8;

  { Indexes of the images used for Test tree checkboxes }
  imgDISABLED        = 0;
  imgPARENT_DISABLED = 1;
  imgENABLED         = 2;
  imgEXCLUDED        = 3;
  imgPARENT_EXCLUDED = 4;


procedure RunTest(Test: ITestProxy);
begin
  fpgApplication.Initialize;
  with TGUITestRunner.Create(nil) do
  begin
    try
      Suite := Test;
      Show;
      fpgApplication.Run;
    finally
      Free;
    end;
    if Assigned(Test) then
      Test.ReleaseTests;
  end;
end;

procedure RunRegisteredTests;
begin
  RunTest(RegisteredTests);
end;

{ Get rid of any control characters. Anything smaller than $1F. Also replace
  LineEnding characters with a space, to tidy up the resulting string. }
function DeControl(const AString: string): string;
var
  i: Integer;
  LChr: Char;
  s: string;
begin
  Result := '';
  if AString = '' then
    Exit;

  s := StringReplace(AString, LineEnding, ' ', [rfReplaceAll]);
  for i := 1 to Length(s) do
  begin
    LChr := s[i];
    if Ord(LChr) > $1F then
      Result := Result + LChr;
  end;
end;

procedure TGUITestRunner.TestNodeSelected(Sender: TObject);
var
  i: integer;
  obj: TFailureDataObject;
begin
  for i := 0 to FailureGrid.RowCount-1 do
  begin
    obj := FailureGrid.Objects[0, i] as TFailureDataObject;
    if obj.TreeNode = TestTree.Selection then
    begin
      FailureGrid.FocusRow := i;
      break;
    end;
  end;
end;

procedure TGUITestRunner.Status(const ATest: ITestProxy; AMessage: string);
begin
  if meErrorMessage.Lines.Count = 0 then
    meErrorMessage.Lines.Add(ATest.Name + ':');

  meErrorMessage.Lines.Add(AMessage);
end;

procedure TGUITestRunner.AddSuccess(ATest: ITestProxy);
var
  LOverridesGUI: Boolean;
  LHasRunTimePropsSet: Boolean;
begin
  {$IFDEF gDEBUG}
  SendDebug('success: ' + ATest.Name);
  Assert(Assigned(ATest));
  {$ENDIF}
  if not IsTestMethod(ATest) then
    SetTreeNodeImage(TestToNode(ATest), imgRUN)
  else
  begin
    FTestFailed := False;
    if miShowOverriddenFailures.Checked then
    begin
      LOverridesGUI :=
//        (
        (miFailTestIfNoChecks.Checked and not ATest.FailsOnNoChecksExecuted);
//         or (FailTestCaseIfMemoryLeakedMenuItem.Checked and not ATest.FailsOnMemoryLeak)) or
//         (FailTestCaseIfMemoryLeakedMenuItem.Checked and
//            IgnoreMemoryLeakInSetUpTearDownMenuItem.Checked and
//              not ATest.IgnoreSetUpTearDownLeaks) or
//        ATest.LeakAllowed;

      ATest.IsOverridden := LOverridesGUI;

      if LOverridesGUI then
      begin
        if ATest.IsWarning then
          SetTreeNodeImage(TestToNode(ATest), imgWARNFAIL)
        else
          SetTreeNodeImage(TestToNode(ATest), imgOVERRIDE)
      end
      else
        SetTreeNodeImage(TestToNode(ATest), imgRUN);
    end
    else if miShowOverriddenFailures.Checked then
    begin
      LHasRunTimePropsSet :=
//        (
         (ATest.FailsOnNoChecksExecuted and not miFailTestIfNoChecks.Checked);
         //or (ATest.FailsOnMemoryLeak and not FailTestCaseIfMemoryLeakedMenuItem.Checked) or
         //(FailTestCaseIfMemoryLeakedMenuItem.Checked and
         // (not IgnoreMemoryLeakInSetUpTearDownMenuItem.Checked and
         //   ATest.IgnoreSetUpTearDownLeaks)) or
         //(ATest.AllowedMemoryLeakSize <> 0));

      if LHasRunTimePropsSet then
        SetTreeNodeImage(TestToNode(ATest), imgOVERRIDE)
      else
        SetTreeNodeImage(TestToNode(ATest), imgRUN);
    end
    // TODO: graeme
    //else if ShowWarnedTestToolButton.Down and ATest.IsWarning then
    //  SetTreeNodeImage(TestToNode(ATest), imgWARNING)
    //else if ShowSummaryLevelExitsToolButton.Down and ATest.EarlyExit then
    //  SetTreeNodeImage(TestToNode(ATest), imgEARLYEXIT)
    else
      SetTreeNodeImage(TestToNode(ATest), imgRUN);
  end;
end;

procedure TGUITestRunner.AddError(AError: TTestFailure);
var
  newrow: integer;
begin
  {$IFDEF gDEBUG}
  SendDebug('error: ' + AError.FailedTest.Name);
  {$ENDIF}
  FTestFailed := True;

  newrow := AddFailureItem(AError);
  TFailureDataObject(FailureGrid.Objects[0, newrow]).ImageIndex := imgERROR;

  SetProgressBarColor(clERROR);
  SetTreeNodeImage(TestToNode(AError.FailedTest), imgERROR);
  UpdateStatus(True);
end;

procedure TGUITestRunner.AddFailure(AFailure: TTestFailure);
var
  newrow: integer;
begin
  {$IFDEF gDEBUG}
  SendDebug('failure: ' + AFailure.FailedTest.Name);
  {$ENDIF}
  FTestFailed := True;

  newrow := AddFailureItem(AFailure);
  TFailureDataObject(FailureGrid.Objects[0, newrow]).ImageIndex := imgFAILED;

  // Override higher priority error colour if needed
  if TestResult.ErrorCount = 0 then
    SetProgressBarColor(clFAILURE);

  SetTreeNodeImage(TestToNode(AFailure.FailedTest), imgFAILED);
  UpdateStatus(True);
end;

procedure TGUITestRunner.AddWarning(AWarning: TTestFailure);
var
  newrow: integer;
begin
  {$IFDEF gDEBUG}
  SendDebug('warning: ' + AWarning.FailedTest.Name);
  {$ENDIF}
  if miEnableWarnings.Checked then
  begin
    newrow := AddFailureItem(AWarning);
    TFailureDataObject(FailureGrid.Objects[0, newrow]).ImageIndex := imgWARNING;

    SetTreeNodeImage(TestToNode(AWarning.failedTest), imgWARNING);
    UpdateStatus(True);
  end
  else
    AddSuccess(AWarning.FailedTest);
end;

procedure TGUITestRunner.TestingStarts;
begin
  {$IFDEF gDEBUG}
  SendDebug('*** Testing Starts ****');
  {$ENDIF}
  FTotalTime := 0;
  UpdateStatus(True);
  SetProgressBarColor(clOK);
  ClearStatusMessage;
end;

procedure TGUITestRunner.StartTest(Test: ITestProxy);
var
  Node: TfpgTreeNode;
begin
  {$IFDEF gDEBUG}
  assert(assigned(TestResult));
  assert(assigned(Test));
  {$ENDIF}

  Node := TestToNode(Test);

  {$IFDEF gDEBUG}
  assert(assigned(Node));
  {$ENDIF}

  SetTreeNodeImage(Node, imgRUNNING);

  if miShowTestedNode.Checked then
    MakeNodeVisible(Node);

  ClearFailureMessage;
  UpdateStatus(False);
end;

procedure TGUITestRunner.EndTest(Test: ITestProxy);
begin
  UpdateStatus(False);
end;

procedure TGUITestRunner.TestingEnds(TestResult: ITestResult);
var
  idx: Integer;
  AProxy: ITestProxy;
begin
  {$IFDEF gDEBUG}
  SendSeparator;
  {$ENDIF}
  for idx := 0 to FTests.Count-1 do
  begin
    AProxy := (FTests[idx] as ITestProxy);
    AProxy.IsOverridden := False;
  end;
  FTotalTime := TestResult.TotalTime;
  UpdateStatus(True);
end;

function TGUITestRunner.ShouldRunTest(const ATest: ITestProxy): Boolean;
begin
  Result := not ATest.Excluded;
  if Result and (FSelectedTests <> nil) then
    Result := FSelectedTests.IndexOf(ATest as ITestProxy) >= 0;
end;

procedure TGUITestRunner.StartSuite(Suite: ITestProxy);
begin
  // nothing needed to be done
end;

procedure TGUITestRunner.EndSuite(Suite: ITestProxy);
begin
  // nothing needed to be done
end;

{ A single event handler that handles all Command based events. }
procedure TGUITestRunner.CommandHandler(Sender: TObject);
var
  cmd: ICommand;
  holder: ICommandHolder;
begin
  if Supports(Sender, ICommandHolder, holder) then
  begin
    cmd := holder.GetCommand;
    cmd.Execute;
  end;
end;

procedure TGUITestRunner.FormCreate(Sender: TObject);
begin
  LoadConfiguration;
  Setup;
  HoldOptions(False);
end;

procedure TGUITestRunner.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  ClearResult;

  TestTree.ImageList := nil;
  for i := FImageList.Count-1 downto 0 do
    FImageList[i].Image := nil; // clear the references
  FImageList.Free;

  TestTree.StateImageList := nil;
  for i := FStateImageList.Count-1 downto 0 do
    FStateImageList[i].Image := nil;  // clear the references
  FStateImageList.Free;

  FreeAndNil(FTests); // Note this is an object full of Interface refs
  Suite := nil;       // Take down the test proxys
//  ClearRegistry;      // Take down the Registered tests
end;

procedure TGUITestRunner.FormShow(Sender: TObject);
begin
  FUpdateTimer := TfpgTimer.Create(300);
  FUpdateTimer.OnTimer := @OnTimerFired;
  FUpdateTimer.Enabled := True;


  ResultsView.Cells[0, 0] := 'Tests';
  ResultsView.Cells[1, 0] := 'Run';
  ResultsView.Cells[2, 0] := 'Failures';
  ResultsView.Cells[3, 0] := 'Errors';
  ResultsView.Cells[4, 0] := 'Warnings';
  ResultsView.Cells[5, 0] := 'Test Time';
  ResultsView.Cells[6, 0] := 'Total Time';

  SetupGUINodes;
  TestTree.SetFocus;
end;

procedure TGUITestRunner.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.ProcessClickOnStateIcon(Sender: TObject; ANode: TfpgTreeNode);
begin
  if FRunning then
    Exit;
  SwitchNodeState(ANode);
end;

procedure TGUITestRunner.ProcessKeyPressOnTreeview(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  if (KeyCode = keySpace) and (TestTree.Selection <> nil) then
  begin
    SwitchNodeState(TestTree.Selection);
    UpdateStatus(True);
  end;
end;

procedure TGUITestRunner.DrawGridCell(Sender: TObject; const ARow, ACol: Integer;
  const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
begin
  if ARow = 0 then
  begin
    ResultsView.Canvas.SetColor(clDarkGray);
    ResultsView.Canvas.FillRectangle(ARect);
  end;
end;

procedure TGUITestRunner.OnTimerFired(Sender: TObject);
begin
  FTimerExpired := True;
  FUpdateTimer.Enabled := False;
end;

function TGUITestRunner.get_TestResult: ITestResult;
begin
  Result := FTestResult;
end;

procedure TGUITestRunner.SetSuite(const AValue: ITestProxy);
begin
  if FSuite <> nil then
    FSuite.ReleaseTests;

  FSuite := AValue;
  if FSuite <> nil then
  begin
    LoadSuiteConfiguration;
    EnableUI(True);
    InitTree;
  end
  else
    EnableUI(False);
end;

procedure TGUITestRunner.set_TestResult(const AValue: ITestResult);
begin
  FTestResult := AValue;
end;

procedure TGUITestRunner.LoadSuiteConfiguration;
begin
  if Suite <> nil then
    Suite.LoadConfiguration(IniFileName, False {UseRegistryAction.Checked}, True);
end;

procedure TGUITestRunner.EnableUI(AEnable: Boolean);
begin
{
  SelectAllAction.Enabled    := enable;
  DeselectAllAction.Enabled  := enable;
  SelectFailedAction.Enabled := enable;
  SelectCurrentAction.Enabled := enable;
  DeselectCurrentAction.Enabled := enable;
  HideTestNodesAction.Enabled   := enable;
  ExpandAllNodesAction.Enabled  := enable;
}
end;

function TGUITestRunner.IniFileName: TfpgString;
begin
  result := fpgGetAppConfigDir(False) + TEST_INI_FILE;
  {$IFDEF gDEBUG}
  SendDebug('IniFileName = ' + Result);
  {$ENDIF}
end;

procedure TGUITestRunner.InitCommands;
begin
  // The menu item File|Quit shares the command of btnQuit
  mnuFile.MenuItemByName('Quit').SetCommand(TExitCommand.Create(self));

  SelectAllCommand := TExitCommand.Create(self);
  HideTestNodesCommand := THideTestNodesOnOpenCommand.Create(self);
  ExpandAllNodesCommand := TExpandAllNodesCommand.Create(self);
  miTreeSelectAll.SetCommand(TSelectAllCommand.Create(self));
  miTreeDeselectAll.SetCommand(TDeselectAllCommand.Create(self));
  miTreeSelectFailed.SetCommand(TSelectFailedCommand.Create(self));

  btnSelectFailed.SetCommand(TSelectFailedCommand.Create(self));
  btnDeselectCurrent.SetCommand(TDeselectCurrentCommand.Create(self));
  btnSelectCurrent.SetCommand(TSelectCurrentCommand.Create(self));
  btnRunAllSelected.SetCommand(TRunAllSelectedCommand.Create(self));
  btnRunCurrentSelection.SetCommand(TRunCurrentSelectionCommand.Create(self));
  btnStopTests.SetCommand(TStopTestsCommand.Create(self));
  btnSelectAll.SetCommand(TSelectAllCommand.Create(self));
  btnSelectNone.SetCommand(TDeselectAllCommand.Create(self));

  miRunAllSelected.SetCommand(TRunAllSelectedCommand.Create(self));
  miRunCurrentSelection.SetCommand(TRunCurrentSelectionCommand.Create(self));
end;

function TGUITestRunner.EnableTest(Test: ITestProxy): boolean;
begin
  Test.Enabled := True;
  Result := True;
end;

function TGUITestRunner.DisableTest(Test: ITestProxy): boolean;
begin
  Test.Enabled := False;
  Result := True;
end;

procedure TGUITestRunner.ApplyToTests(root: TfpgTreeNode; const func: TTestFunc);

  procedure DoApply(RootNode: TfpgTreeNode);
  var
    Test: ITestProxy;
    Node: TfpgTreeNode;
  begin
    if RootNode <> nil then
    begin
      Test := NodeToTest(RootNode);
      if func(Test) then
      begin
        Node := RootNode.FirstSubNode;
        while Node <> nil do
        begin
          DoApply(Node);
          Node := Node.Next;
        end;
      end;
    end;
  end;
begin
  try
    DoApply(root)
  finally
    TestTree.Invalidate;
  end;
  TotalTestsCount := (FSuite as ITestProxy).CountEnabledTestCases;
  UpdateTestTreeState;
end;

function TGUITestRunner.AddFailureItem(Failure: TTestFailure): integer;
var
  obj: TFailureDataObject;
begin
  {$IFDEF gDEBUG}
  Assert(Assigned(Failure));
  {$ENDIF}
  Result := FailureGrid.RowCount;
  FailureGrid.RowCount := Result + 1;

  obj := TFailureDataObject.Create;
  obj.ImageIndex := -1;
  obj.TreeNode := TestToNode(Failure.FailedTest);
  obj.Caption := Failure.FailedTest.Name;                                // Caption
  obj.ThrownExceptionName := Failure.ThrownExceptionName;               // exception type
  obj.FilteredErrorMessage := TrimLeft(DeControl(Failure.ThrownExceptionMessage)); // filtered errormessage
  obj.LocationInfo := Failure.LocationInfo;                              // unit name and line number
  // These are not shown in display
  obj.FullTestPath := Failure.FailedTest.ParentPath + '.';               // full path to test method
  obj.ErrorMessage := Failure.ThrownExceptionMessage;                    // unfiltered errormessage

  FailureGrid.Objects[0, Result] := obj;
  FailureGrid.Cells[0, Result] := obj.Caption;
  FailureGrid.Cells[1, Result] := obj.ThrownExceptionName;
  FailureGrid.Cells[2, Result] := obj.FilteredErrorMessage;

//  {$IFDEF USE_JEDI_JCL}
//    Item.SubItems.Add(Failure.StackTrace);                      // 5 stack trace if collected
//  {$ENDIF}
end;

procedure TGUITestRunner.UpdateStatus(const FullUpdate: Boolean);
var
  i: Integer;
  TestNumber: Integer;
begin
  if FullUpdate then
  begin
    if Assigned(Suite) then
      ResultsView.Cells[0, 1] := IntToStr(TotalTestsCount)
    else
      ResultsView.Cells[0, 1] := '';
  end;

  if TestResult <> nil then
  begin
    // Save the test number as we use it a lot
    TestNumber := TestResult.RunCount;
    FTotalTime := TestResult.TotalTime;
    if FullUpdate or FTimerExpired or ((TestNumber and 15) = 0) or FTestFailed then
    begin
      with ResultsView do
      begin
        Cells[1, 1] := IntToStr(TestNumber);
        Cells[2, 1] := IntToStr(TestResult.FailureCount);
        Cells[3, 1] := IntToStr(TestResult.ErrorCount);
        Cells[4, 1] := IntToStr(TestResult.WarningCount + TestResult.Overrides);
        Cells[5, 1] := ElapsedDHMS(TestResult.TotalTime);
        Cells[6, 1] := ElapsedDHMS(FTotalTime);
      end;
      with TestResult do
      begin
        ScoreBar.Progress  := TestNumber - (FailureCount + ErrorCount);
        ProgressBar.Progress := TestNumber;

        // There is a possibility for zero tests
        //if (TestNumber = 0) and (TotalTestsCount = 0) then
        //  LbProgress.Caption := '100%'
        //else
        //  LbProgress.Caption := IntToStr((100 * ScoreBar.Position) div ScoreBar.Max) + '%';
      end;
      if (TestNumber < TotalTestsCount) then
      begin
        FTimerExpired := False;
        FUpdateTimer.Enabled := True;
      end;
    end;
  end
  else
  begin  {TestResult = nil}
    with ResultsView do
    begin
      if (Cells[0, 1] = '0') or (Cells[0, 1] = '') then
      begin
        for i := 1 to 6 do
          Cells[i, 1] := ''
      end
      else
      begin
        Cells[5, 1] := ElapsedDHMS(SelectedTest.ElapsedTestTime);
        Cells[6, 1] := ElapsedDHMS(FTotalTime);
      end;
    end;

    ResetProgress;
  end;

  fpgApplication.ProcessMessages;
end;

procedure TGUITestRunner.FillTestTree(ARootNode: TfpgTreeNode; ATest: ITestProxy);
var
  TestTests: IInterfaceList;
  i: Integer;
begin
  if ATest = nil then
    exit;

  if ARootNode = nil then
    ARootNode := TestTree.RootNode.AppendText(ATest.Name)
  else
    ARootNode := ARootNode.AppendText(ATest.Name);

  ARootNode.Data := Pointer(ATest);

  TestTests := ATest.Tests;
  for i := 0 to TestTests.Count-1 do
    FillTestTree(ARootNode, TestTests[i] as ITestProxy);
end;

procedure TGUITestRunner.FillTestTree(ATest: ITestProxy);
begin
  TestTree.RootNode.Clear;
  FTests.Clear;
  FillTestTree(nil, Suite);
end;

procedure TGUITestRunner.SetUp;
var
  i: Integer;
  Node: TfpgTreeNode;
begin
  ClearFailureGrid;
  ResetProgress;
  fpgApplication.ProcessMessages;

  with ResultsView do
  begin
    Cells[0, 1] := '';    //Test Count
    Cells[1, 1] := '';    //Tests Run
    Cells[2, 1] := '';    //Failures
    Cells[3, 1] := '';    //Errors
    Cells[4, 1] := '';    //Warnings
    Cells[5, 1] := '';    //Test's Time
    Cells[6, 1] := '';    //Total Test Time

    if Suite <> nil then
    begin
      TotalTestsCount := Suite.CountEnabledTestCases;
      Cells[0, 1] := IntToStr(TotalTestsCount);
      ProgressBar.MaxValue := TotalTestsCount;
    end
    else
      ProgressBar.MaxValue := 10000;

    ScoreBar.MaxValue := ProgressBar.MaxValue;
  end;

  Node := TestTree.RootNode.FirstSubNode;
  while Node <> nil do
  begin
    Node.ImageIndex := imgNone;
    Node := TestTree.NextNode(Node);
  end;

  UpdateTestTreeState;
end;

function TGUITestRunner.NodeToTest(ANode: TfpgTreeNode): ITestProxy;
begin
  Result := nil;
  if not Assigned(ANode) then
    Exit;
  Result := ITestProxy(ANode.Data);
end;

function TGUITestRunner.TestToNode(Test: ITestProxy): TfpgTreeNode;
begin
  Result := nil;
  if not Assigned(Test) then
    Exit;

  if Assigned(Test.GUIObject) then
    Result := Test.GUIObject as TfpgTreeNode;
end;

function TGUITestRunner.SelectedTest: ITestProxy;
begin
  if TestTree.Selection = nil then
    Result := nil
  else
    Result := NodeToTest(TestTree.Selection);
end;

procedure TGUITestRunner.ListSelectedTests;
var
  ATest: ITestProxy;
  ANode: TfpgTreeNode;
begin
  FSelectedTests.Free;
  FSelectedTests := nil;
  FSelectedTests := TInterfaceList.Create;

  ANode := TestTree.Selection;

  while Assigned(ANode) do
  begin
    ATest := NodeToTest(ANode);
    FSelectedTests.Add(ATest as ITestProxy);
    ANode := ANode.Parent;
    if ANode = TestTree.RootNode then
      ANode := nil;
  end;
end;

procedure TGUITestRunner.SwitchNodeState(ANode: TfpgTreeNode);
begin
  {$IFDEF gDEBUG}
  Assert(ANode <> nil);
  {$ENDIF}
  TestTree.BeginUpdate;
  try
    SetNodeState(ANode, not NodeToTest(ANode).Enabled);
  finally
    TestTree.EndUpdate;
  end;
end;

procedure TGUITestRunner.UpdateTestTreeState;
var
  Node: TfpgTreeNode;
begin
  TestTree.BeginUpdate;
  if TestTree.RootNode.Count > 0 then
  begin
    try
      Node := TestTree.RootNode.FirstSubNode;
      while Node <> nil do
      begin
        UpdateNodeState(Node);
        Node := Node.Next;
      end
    finally
      TestTree.Invalidate;
    end;
  end;
  TestTree.EndUpdate;
end;

procedure TGUITestRunner.SetNodeState(ANode: TfpgTreeNode; AEnabled: boolean);
var
  MostSeniorChanged: TfpgTreeNode;
  n: TfpgTreeNode;
begin
  {$IFDEF gDEBUG}
  Assert(ANode <> nil);
  {$ENDIF}

  if (NodeToTest(ANode).Enabled <> AEnabled) then
    NodeToTest(ANode).Enabled := AEnabled;

  MostSeniorChanged := ANode;
  if AEnabled then
  begin
    n := ANode;
    while HasParentNode(n) do
    begin
      n := n.Parent;
      if not NodeToTest(n).Enabled then
      begin // changed
        NodeToTest(n).Enabled := True;
        MostSeniorChanged := n;
        UpdateNodeImage(n);
      end
    end;
  end;

  UpdateNodeState(MostSeniorChanged);
  TestTree.Invalidate;
  RefreshTestCount;
end;

procedure TGUITestRunner.UpdateNodeImage(ANode: TfpgTreeNode);
var
  Test: ITestProxy;
begin
  Test := NodeToTest(ANode);
  if not Test.Enabled then
  begin
    ANode.StateImageIndex := imgDISABLED;
  end
  else if HasParentNode(ANode) and (ANode.Parent.StateImageIndex <= imgPARENT_DISABLED) then
  begin
    ANode.StateImageIndex := imgPARENT_DISABLED;
  end
  else
  begin
    ANode.StateImageIndex := imgENABLED;
  end;

  if HasParentNode(ANode) and (ANode.Parent.StateImageIndex >= imgEXCLUDED) then
      ANode.StateImageIndex := imgPARENT_EXCLUDED
  else if Test.Excluded then
    ANode.StateImageIndex := imgEXCLUDED;

  TestTree.Invalidate;
end;

procedure TGUITestRunner.UpdateNodeState(ANode: TfpgTreeNode);
var
  Test: ITestProxy;
  n: TfpgTreeNode;
begin
  {$IFDEF gDEBUG}
  Assert(Assigned(ANode));
  Test := NodeToTest(ANode);
  Assert(Assigned(Test));
  SendDebug(ANode.Text);
  {$ENDIF}

  UpdateNodeImage(ANode);

  if ANode.Count > 0 then
  begin
    n := ANode.FirstSubNode;
    while n <> nil do
    begin
      UpdateNodeState(n);
      n := n.Next;
    end;
  end;
end;

procedure TGUITestRunner.RefreshTestCount;
begin
  TotalTestsCount := (FSuite as ITestProxy).CountEnabledTestCases;
  if Assigned(Suite) then
    ResultsView.Cells[0, 1] := IntToStr(TotalTestsCount)
  else
    ResultsView.Cells[0, 1] := '';
end;

procedure TGUITestRunner.AutoSaveConfiguration;
begin
  if miAutoSaveConfiguration.Checked then
    SaveConfiguration;
end;

procedure TGUITestRunner.SaveConfiguration;
var
  i: Integer;
begin
  if Suite <> nil then
    Suite.SaveConfiguration(IniFileName, False {UseRegistryAction.Checked}, True);
  SaveFormPlacement;

  { center splitter location information - yes, we don't same the splitter infor itself }
  gINI.WriteInteger(cnConfigIniSection, 'bvlTreeAndProgress.Height', bvlTreeAndProgress.Height);

    { error box }
//    WriteInteger(cnConfigIniSection, 'ErrorMessage.Height',
//      ErrorBoxPanel.Height);
//    WriteBool(cnConfigIniSection, 'ErrorMessage.Visible',
//      ErrorBoxVisibleAction.Checked);

    { failure list configuration }
//    with FailureListView do begin
//      for i := 0 to Columns.Count-1 do
//      begin
//       WriteInteger(cnConfigIniSection,
//                     Format('FailureList.ColumnWidth[%d]', [i]),
//                     Columns[i].Width);
//      end;
//    end;

      { other options }
  gINI.WriteBool(cnConfigIniSection, 'AutoSave',      miAutoSaveConfiguration.Checked);
  gINI.WriteBool(cnConfigIniSection, 'HideTestNodesOnOpen',      miHideTestNodesOnOpen.Checked);
//    WriteBool(cnConfigIniSection, 'BreakOnFailures',          BreakOnFailuresAction.Checked);
//    WriteBool(cnConfigIniSection, 'ReportMemoryLeakTypes',    ReportMemoryLeakTypeOnShutdownAction.Checked);
  gINI.WriteBool(cnConfigIniSection, 'SelectTestedNode',         miShowTestedNode.Checked);
//    WriteBool(cnConfigIniSection, 'ShowRunTimeProperties',    ShowTestCasesWithRunTimePropertiesAction.Checked);
//    WriteBool(cnConfigIniSection, 'ShowOverriddenFailures',   ShowOverriddenFailuresAction.Checked);
//    WriteBool(cnConfigIniSection, 'ShowEarlyExitTests',       ShowEarlyExitedTestAction.Checked);

//    WriteInteger(cnConfigIniSection, 'PopupX',                FPopupX);
//    WriteInteger(cnConfigIniSection, 'PopupY',                FPopupY);

    // Settings common to all test runners
//    WriteBool(cnRunners, 'FailOnNoChecksExecuted',    FailIfNoChecksExecutedAction.Checked);
//    WriteBool(cnRunners, 'FailOnMemoryLeaked',        FailTestCaseIfMemoryLeakedAction.Checked);
//    WriteBool(cnRunners, 'EnableWarnings',            EnableWarningsAction.Checked);
//    WriteBool(cnRunners, 'IgnoreSetUpTearDownLeaks',  IgnoreMemoryLeakInSetUpTearDownAction.Checked);
//    WriteBool(cnRunners, 'InhibitSummaryLevelChecks', InhibitSummaryLevelChecksAction.Checked);
end;

procedure TGUITestRunner.LoadConfiguration;
var
  i: Integer;
begin
//  LoadRegistryAction;
  LoadFormPlacement;
  LoadSuiteConfiguration;

  { center splitter location }
  bvlTreeAndProgress.Height := gINI.ReadInteger(cnConfigIniSection, 'bvlTreeAndProgress.Height', bvlTreeAndProgress.Height);
  ClientArea.Realign;

    { error splitter location }
//    with ErrorBoxPanel do
//      Height := ReadInteger(cnConfigIniSection, 'ErrorMessage.Height', Height);
//    with ErrorBoxVisibleAction do
//      Checked := ReadBool(cnConfigIniSection, 'ErrorMessage.Visible', Checked);

//    ErrorBoxSplitter.Visible := ErrorBoxVisibleAction.Checked;
//    ErrorBoxPanel.Visible    := ErrorBoxVisibleAction.Checked;

    { failure list configuration }
//    with FailureListView do begin
//      for i := 0 to Columns.Count-1 do
//      begin
//        Columns[i].Width := Max(4, ReadInteger(cnConfigIniSection,
//                                   Format('FailureList.ColumnWidth[%d]', [i]),
//                                   Columns[i].Width));
//      end;
//    end;

    { other options }
    with miAutoSaveConfiguration do
      Checked := gINI.ReadBool(cnConfigIniSection, 'AutoSave', Checked);
    with miHideTestNodesOnOpen do
      Checked := gINI.ReadBool(cnConfigIniSection, 'HideTestNodesOnOpen', Checked);
    with miShowTestedNode do
      Checked := gINI.ReadBool(cnConfigIniSection, 'SelectTestedNode', Checked);
//    BreakOnFailuresAction.Checked := ReadBool(cnConfigIniSection,
//      'BreakOnFailures', BreakOnFailuresAction.Checked);
//    ReportMemoryLeakTypeOnShutdownAction.Checked := ReadBool(cnConfigIniSection,
//      'ReportMemoryLeakTypes', ReportMemoryLeakTypeOnShutdownAction.Checked);
//    ShowTestCasesWithRunTimePropertiesAction.Checked := ReadBool(cnConfigIniSection,
//      'ShowRunTimeProperties', ShowTestCasesWithRunTimePropertiesAction.Checked);
//    ShowOverriddenFailuresAction.Checked := ReadBool(cnConfigIniSection,
//      'ShowOverriddenFailures', ShowOverriddenFailuresAction.Checked);
//    ShowEarlyExitedTestAction.Checked := ReadBool(cnConfigIniSection,
//      'ShowEarlyExitTests', ShowEarlyExitedTestAction.Checked);

//    FPopupX := ReadInteger(cnConfigIniSection,'PopupX', 350);
//    FPopupY := ReadInteger(cnConfigIniSection,'PopupY', 30);

    // Read settings common to all test runners
//    FailIfNoChecksExecutedAction.Checked := ReadBool(cnRunners,
//      'FailOnNoChecksExecuted', FailIfNoChecksExecutedAction.Checked);
//    FailTestCaseIfMemoryLeakedAction.Checked := ReadBool(cnRunners,
//      'FailOnMemoryLeaked', FailTestCaseIfMemoryLeakedAction.Checked);
//    EnableWarningsAction.Checked := ReadBool(cnRunners,
//      'EnableWarnings', EnableWarningsAction.Checked);
//    InhibitSummaryLevelChecksAction.Checked := ReadBool(cnRunners,
//      'InhibitSummaryLevelChecks', InhibitSummaryLevelChecksAction.Checked);
//    IgnoreMemoryLeakInSetUpTearDownAction.Checked := ReadBool(cnRunners,
//      'IgnoreSetUpTearDownLeaks', IgnoreMemoryLeakInSetUpTearDownAction.Checked);

  if Suite <> nil then
    UpdateTestTreeState;
end;

procedure TGUITestRunner.SaveFormPlacement;
begin
  gINI.WriteInteger(Name + 'State', 'Left', Left);
  gINI.WriteInteger(Name + 'State', 'Top', Top);
  gINI.WriteInteger(Name + 'State', 'Width', Width);
  gINI.WriteInteger(Name + 'State', 'Height', Height);
end;

procedure TGUITestRunner.LoadFormPlacement;
begin
  Left := gINI.ReadInteger(Name + 'State', 'Left', Left);
  Top := gINI.ReadInteger(Name + 'State', 'Top', Top);
  Width := gINI.ReadInteger(Name + 'State', 'Width', Width);
  Height := gINI.ReadInteger(Name + 'State', 'Height', Height);
  UpdateWindowPosition;
end;

function TGUITestRunner.NodeIsGrandparent(ANode: TfpgTreeNode): boolean;
var
  AChildNode: TfpgTreeNode;
begin
  Result := False;
  if ANode.HasChildren then
  begin
    AChildNode := ANode.FirstSubNode;
    while AChildNode <> nil do
    begin
      Result := AChildNode.HasChildren or Result;
      AChildNode := AChildNode.Next;
    end;
  end;
end;

procedure TGUITestRunner.CollapseNonGrandparentNodes(RootNode: TfpgTreeNode);
var
  AChildNode: TfpgTreeNode;
begin
  if not NodeIsGrandparent(RootNode) then
    RootNode.Collapse;

  AChildNode := RootNode.FirstSubNode;
  while AChildNode <> nil do
  begin
    CollapseNonGrandparentNodes(AChildNode);
    AChildNode := AChildNode.Next;
  end;
end;

function TGUITestRunner.HasParentNode(ANode: TfpgTreeNode): Boolean;
begin
  Result := (ANode.Parent <> nil) and (ANode.Parent <> TestTree.RootNode);
end;

procedure TGUITestRunner.SetupGUINodes;
var
  Node: TfpgTreeNode;
  Test: ITestProxy;
begin
  { Set up the GUI nodes in the test nodes.
    This method is also called after loading test libraries }

  Node := TestTree.RootNode.FirstSubNode;
  while Assigned(Node) do
  begin
    // Get and check the test for the tree node

    Test := NodeToTest(Node);
    assert(Assigned(Test));

    // Save the tree node in the test and get the next tree node

    Test.GUIObject := Node;

    Node := TestTree.NextNode(Node);
  end;
end;

procedure TGUITestRunner.MakeNodeVisible(Node: TfpgTreeNode);
begin
  TestTree.Selection := Node;
  TestTree.Invalidate;
end;

procedure TGUITestRunner.SetTreeNodeImage(Node: TfpgTreeNode; imgIndex: Integer);
var
  n: TfpgTreeNode;
begin
  n := Node;
  while n <> nil do
  begin
    if imgIndex > n.ImageIndex then
       n.ImageIndex := imgIndex;
    if imgIndex = imgRUNNING then
      n := nil
    else
      n := n.Parent;
  end;
  TestTree.Invalidate;
end;

procedure TGUITestRunner.ClearStatusMessage;
begin
  meErrorMessage.Clear;
end;

procedure TGUITestRunner.RunTheTest(ATest: ITestProxy);
begin
  if ATest = nil then
    Exit;
  if FRunning then
  begin
    // warning: we're reentering this method if FRunning is true
    Assert(FTestResult <> nil);
    TestResult.Stop;
    Exit;
  end;

  FRunning := True;
  try
// TODO: graemeg
//    RunAction.Enabled  := False;
//    StopAction.Enabled := True;
//    CopyMessageToClipboardAction.Enabled := false;
    EnableUI(false);
    AutoSaveConfiguration;
    ClearFailureMessage;
    TestResult := GetTestResult; // Replaces TTestResult.create
    try
      {$IFDEF XMLLISTENER}
      TestResult.AddListener(
        TXMLListener.Create(fpgGetAppConfigDir(False) + Suite.Name
          {, 'type="text/xsl" href="fpcunit2.xsl"'}));
      {$ENDIF}
      TestResult.AddListener(self);
//      TestResult.BreakOnFailures := BreakOnFailuresAction.Checked;
      LoadSuiteConfiguration;
      ATest.Run(TestResult);
    finally
      TestResult.ReleaseListeners;
      TestResult := nil;
    end;
  finally
    FRunning := false;
    EnableUI(true);
  end;
end;

procedure TGUITestRunner.HoldOptions(const AValue: boolean);
{ Prevents selected options from being changed while executing tests
  but preserves enabled image. }
begin
  FHoldOptions := AValue;
end;

procedure TGUITestRunner.ClearResult;
begin
  // TODO: Double check why FTestResult is always nil just after a test run. Thus causing a memory leak because ClearGridFailures is never called when application terminates.
//  if FTestResult <> nil then
//  begin
    FTestResult := nil;
    ClearFailureMessage;
    ClearFailureGrid;
//  end;
end;

procedure TGUITestRunner.DisplayFailureMessage(ARow: integer);
var
  hlColor :TfpgColor;
  Test    :ITestProxy;
  lStatus  :string;
  obj: TFailureDataObject;
begin
//  TestTree.Selected := TTreeNode(Item.data);
  obj := TFailureDataObject(FailureGrid.Objects[0, ARow]);
  Test := NodeToTest(obj.TreeNode);
  case Ord(Test.ExecutionStatus) of
    0 {_Ready}     : hlColor := clGray;
    1 {_Running}   : hlColor := clNavy;
    2 {_HaltTest}  : hlColor := clBlack;
    3 {_Passed}    : hlColor := clLime;
    4 {_Warning}   : hlColor := clGreen;
    5 {_Stopped}   : hlColor := clBlack ;
    6 {_Failed}    : hlColor := clFuchsia;
    7 {_Break}     : hlColor := clBlack;
    8 {_Error}     : hlColor := clRed;
    else
      hlColor := clPurple;
  end;

  with meErrorMessage do
  begin
    Clear;
(*
    SelAttributes.Size  := self.Font.Size;
    SelAttributes.Style := [fsBold];
    SelText := Item.SubItems[3] + Item.Caption + ': '; //ParentPath + Test name

    SelAttributes.Color := hlColor;
    SelAttributes.Style := [fsBold];
    SelText := Item.SubItems[0];        // Exception type

    Lines.Add('');
    SelAttributes.Size  := 11;
    SelAttributes.Color := clWindowText;
    SelAttributes.Style := [];
    SelText := 'at ' + Item.SubItems[2]; // Location info

    if Item.SubItems[1] <> '' then
    begin
      SelAttributes.Color := clWindowText;
      Lines.Add('');
      SelAttributes.Style := [];
      SelText := Item.SubItems[4]; // unfiltered error message
      SelAttributes.Size  := self.Font.Size;
    end;

    Status := Test.Status;
    if Status <> '' then
    begin
      Lines.Add('');
      Lines.Add('');
      SelAttributes.Style := [fsBold];
      Lines.Add('Status Messages');
      SelAttributes.Style := [];
      Lines.Add(Status);
    end;
*)
    { ParentPath + Test name + Exception name }
    Lines.Add(obj.FullTestPath + obj.Caption + ': ' + obj.ThrownExceptionName);
    { location }
    Lines.Add('at ' + obj.LocationInfo);
    { unfiltered error messag }
    Text := Text + LineEnding + (obj.ErrorMessage);

    lStatus := Test.Status;
    if lStatus <> '' then
    begin
      Lines.Add('');
      Lines.Add('');
      Lines.Add('Status Messages:');
      Lines.Add(lStatus);
    end;
(*
{$IFDEF USE_JEDI_JCL}
      if Item.SubItems[5] <> '' then
      begin
        Lines.Add('');
        SelAttributes.Style := [fsBold];
        Lines.Add('StackTrace');
        SelAttributes.Style := [];
        SelText := Item.SubItems[5];
      end;
{$ENDIF}
*)
  end;  { with ErrorMessageRTF... }
end;

procedure TGUITestRunner.ClearFailureMessage;
begin
  meErrorMessage.Clear;
end;

procedure TGUITestRunner.ResetProgress;
begin
  SetProgressBarColor(TfpgColor($c4c4c4));
  ScoreBar.Progress := 0;
  ProgressBar.Progress := 0;
//  LbProgress.Caption := '';
end;

procedure TGUITestRunner.SetProgressBarColor(const AColor: TfpgColor);
begin
  ScoreBar.Color := AColor;
  ScoreBar.Invalidate;
end;

procedure TGUITestRunner.InitTree;
begin
  FTests.Clear;
  FillTestTree(Suite);
  SetUp;

  if miHideTestNodesOnOpen.Checked then
    HideTestNodesCommand.Execute
  else
    ExpandAllNodesCommand.Execute;

  TestTree.Selection := TestTree.RootNode.FirstSubNode;
  TestTree.SetFocus;
end;

constructor TGUITestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTests := TInterfaceList.Create;

  InitializeCustomImages;

  FImageList := TfpgImageList.Create;
  FImageList.AddImage(fpgImages.GetImage('usr.tree0'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree1'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree2'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree3'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree4'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree5'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree6'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree7'));
  FImageList.AddImage(fpgImages.GetImage('usr.tree8'));

  FStateImageList := TfpgImageList.Create;
  FStateImageList.AddImage(fpgImages.GetImage('usr.state0'));  // unchecked
  FStateImageList.AddImage(fpgImages.GetImage('usr.state2'));  // checked disabled
  FStateImageList.AddImage(fpgImages.GetImage('usr.state1'));  // checked
  FStateImageList.AddImage(fpgImages.GetImage('usr.state3'));  // exclude x
  FStateImageList.AddImage(fpgImages.GetImage('usr.state4'));  // exclude x disabled
end;

procedure TGUITestRunner.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: GUITestRunner}
  Name := 'GUITestRunner';
  SetPosition(467, 182, 548, 581);
  WindowTitle := 'GUI Test Runner';
  Hint := '';
  IconName := '';
  ShowHint := True;
  WindowPosition := wpUser;
  MinWidth := 540;
  MinHeight := 450;
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;
  OnShow := @FormShow;
  OnClose := @FormClose;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 548, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  Toolbar := TfpgBevel.Create(self);
  with Toolbar do
  begin
    Name := 'Toolbar';
    SetPosition(0, 24, 548, 30);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsBottomLine;
    Style := bsLowered;
  end;

  btnSelectAll := TfpgButton.Create(Toolbar);
  with btnSelectAll do
  begin
    Name := 'btnSelectAll';
    SetPosition(4, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'usr.selectall';
    TabOrder := 1;
    Focusable := False;
  end;

  btnSelectNone := TfpgButton.Create(Toolbar);
  with btnSelectNone do
  begin
    Name := 'btnSelectNone';
    SetPosition(28, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'usr.selectnone';
    TabOrder := 2;
    Focusable := False;
  end;

  Bevel1 := TfpgBevel.Create(Toolbar);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(57, 2, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  btnSelectFailed := TfpgButton.Create(Toolbar);
  with btnSelectFailed do
  begin
    Name := 'btnSelectFailed';
    SetPosition(64, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Select all failed tests';
    ImageMargin := 0;
    ImageName := 'usr.selectfailedtests';
    TabOrder := 4;
    Focusable := False;
  end;

  btnSelectCurrent := TfpgButton.Create(Toolbar);
  with btnSelectCurrent do
  begin
    Name := 'btnSelectCurrent';
    SetPosition(88, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Select current test';
    ImageMargin := 0;
    ImageName := 'usr.state1';
    TabOrder := 6;
    Focusable := False;
  end;

  btnDeselectCurrent := TfpgButton.Create(Toolbar);
  with btnDeselectCurrent do
  begin
    Name := 'btnDeselectCurrent';
    SetPosition(112, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Deselect current test';
    ImageMargin := 0;
    ImageName := 'usr.state0';
    TabOrder := 5;
    Focusable := False;
  end;

  Bevel2 := TfpgBevel.Create(Toolbar);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(140, 2, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  btnRunAllSelected := TfpgButton.Create(Toolbar);
  with btnRunAllSelected do
  begin
    Name := 'btnRunAllSelected';
    SetPosition(148, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Run all selected tests';
    ImageMargin := 0;
    ImageName := 'usr.starttest';
    TabOrder := 8;
    Focusable := False;
  end;

  btnRunCurrentSelection := TfpgButton.Create(Toolbar);
  with btnRunCurrentSelection do
  begin
    Name := 'btnRunCurrentSelection';
    SetPosition(172, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Run the current test';
    ImageMargin := 0;
    ImageName := 'usr.startselectedtest';
    TabOrder := 9;
    Focusable := False;
  end;

  btnStopTests := TfpgButton.Create(Toolbar);
  with btnStopTests do
  begin
    Name := 'btnStopTests';
    SetPosition(196, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Stop Testing';
    ImageMargin := 0;
    ImageName := 'usr.stoptests';
    TabOrder := 10;
    Focusable := False;
  end;

  Bevel3 := TfpgBevel.Create(Toolbar);
  with Bevel3 do
  begin
    Name := 'Bevel3';
    SetPosition(224, 2, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  Button9 := TfpgButton.Create(Toolbar);
  with Button9 do
  begin
    Name := 'Button9';
    SetPosition(232, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 12;
    Focusable := False;
  end;

  Button10 := TfpgButton.Create(Toolbar);
  with Button10 do
  begin
    Name := 'Button10';
    SetPosition(256, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 13;
    Focusable := False;
  end;

  Button11 := TfpgButton.Create(Toolbar);
  with Button11 do
  begin
    Name := 'Button11';
    SetPosition(280, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 14;
    Focusable := False;
  end;

  Bevel4 := TfpgBevel.Create(Toolbar);
  with Bevel4 do
  begin
    Name := 'Bevel4';
    SetPosition(308, 2, 6, 24);
    Hint := '';
    Shape := bsLeftLine;
    Style := bsLowered;
  end;

  Button12 := TfpgButton.Create(Toolbar);
  with Button12 do
  begin
    Name := 'Button12';
    SetPosition(316, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 16;
    Focusable := False;
  end;

  Button13 := TfpgButton.Create(Toolbar);
  with Button13 do
  begin
    Name := 'Button13';
    SetPosition(340, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 17;
    Focusable := False;
  end;

  Button14 := TfpgButton.Create(Toolbar);
  with Button14 do
  begin
    Name := 'Button14';
    SetPosition(364, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 18;
    Focusable := False;
  end;

  Button15 := TfpgButton.Create(Toolbar);
  with Button15 do
  begin
    Name := 'Button15';
    SetPosition(388, 2, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 19;
    Focusable := False;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 56, 176, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Test Hierarchy:';
  end;

  ClientArea := TfpgBevel.Create(self);
  with ClientArea do
  begin
    Name := 'ClientArea';
    SetPosition(4, 74, 541, 503);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  bvlTreeAndProgress := TfpgBevel.Create(ClientArea);
  with bvlTreeAndProgress do
  begin
    Name := 'bvlTreeAndProgress';
    SetPosition(2, 2, 537, 394);
    Align := alTop;
    Hint := '';
    Shape := bsSpacer;
    Style := bsLowered;
    MinHeight := 150;
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(400, 84, 120, 20);
    AddMenuItem('Quit', 'Esc', nil);
  end;

  mnuTestTree := TfpgPopupMenu.Create(self);
  with mnuTestTree do
  begin
    Name := 'mnuTestTree';
    SetPosition(400, 108, 120, 20);
    miTreeSelectAll := AddMenuItem('Select All', 'Ctrl+A', nil);
    miTreeDeselectAll := AddMenuItem('Deselect All', 'Ctrl+D', nil);
    miTreeSelectFailed := AddMenuItem('Select Failed', 'Ctrl+F', nil);
  end;

  mnuOptions := TfpgPopupMenu.Create(self);
  with mnuOptions do
  begin
    Name := 'mnuOptions';
    SetPosition(400, 132, 120, 20);
    miAutoSaveConfiguration := AddMenuItem('Auto Save Configuration', '', @AutoSaveActionExecute);
    miErrorBoxVisible := AddMenuItem('Error Box Visible', '', nil);
    miAutoChangeFocus := AddMenuItem('Auto Change Focus', '', nil);
    miHideTestNodesOnOpen := AddMenuItem('Hide Test Nodes On Open', '', @HideTestNodesOnOpenClicked);
    miShowTestedNode := AddMenuItem('Show Tested Node', '', @ShowTestedNodeClicked);
    miBreakOnFailure := AddMenuItem('Break On Failures', '', nil);
    AddSeparator;
    miShowTestCasesWithRuntimeProperties := AddMenuItem('Show TestCases with RunTime Properties', '', nil);
    miShowOverriddenFailures := AddMenuItem('Show Overridden Failures', '', nil);
    miShowExitedEarly := AddMenuItem('Show passing Summary Level Tests which exited early', '', nil);
    AddSeparator;
    miFailTestIfNoChecks := AddMenuItem('Fail TestCase if no checks performed', '', nil);
    miEnableWarnings := AddMenuItem('Enable Warnings', '', @EnableWarningsActionExecute);
    miInhibitSummaryLevelChecks := AddMenuItem('Inhibit Summary Level Checking', '', nil);
  end;

  mnuActions := TfpgPopupMenu.Create(self);
  with mnuActions do
  begin
    Name := 'mnuActions';
    SetPosition(400, 156, 120, 20);
    miRunAllSelected := AddMenuItem('Run', 'F9', nil);
    miRunCurrentSelection := AddMenuItem('Run focused test', 'F8', nil);
    AddMenuItem('[todo]', '', nil);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(400, 180, 120, 20);
    AddMenuItem('About fpGUI Toolkit', '', @miHelpAboutFPGui);
    AddMenuItem('About FPTest...', '', @miHelpAboutFPTest);
  end;

  Bevel5 := TfpgBevel.Create(bvlTreeAndProgress);
  with Bevel5 do
  begin
    Name := 'Bevel5';
    SetPosition(2, 201, 533, 191);
    Align := alBottom;
    Hint := '';
    Shape := bsSpacer;
    Style := bsLowered;
  end;

  TestTree := TfpgTreeView.Create(bvlTreeAndProgress);
  with TestTree do
  begin
    Name := 'TestTree';
    SetPosition(2, 2, 533, 199);
    Align := alClient;
    FontDesc := '#Label1';
    Hint := '';
    ShowImages := True;
    TabOrder := 8;
    ImageList := FImageList;
    StateImageList := FStateImageList;
    OnStateImageClicked  := @ProcessClickOnStateIcon;
    OnKeyPress  := @ProcessKeyPressOnTreeview;
    OnChange := @TestNodeSelected;
  end;

  ProgressBar := TfpgGauge.Create(Bevel5);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    SetPosition(96, 5, 424, 15);
    Color := TfpgColor($C4C4C4);
    Hint := '';
    Progress := 90;
  end;

  ScoreBar := TGUIRunnerGauge.Create(Bevel5);
  with ScoreBar do
  begin
    Name := 'ScoreBar';
    SetPosition(96, 24, 424, 15);
    Color := TfpgColor($C4C4C4);
    Hint := '';
    Progress := 75;
  end;

  Label2 := TfpgLabel.Create(Bevel5);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(4, 3, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Progress:';
  end;

  Label3 := TfpgLabel.Create(Bevel5);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(4, 20, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Score:';
  end;

  ResultsView := TfpgStringGrid.Create(Bevel5);
  with ResultsView do
  begin
    Name := 'ResultsView';
    SetPosition(7, 44, 512, 36);
    BackgroundColor := TfpgColor($80000002);
    BorderStyle := ebsNone;
    AddColumn('Tests', 65, taCenter);
    AddColumn('Run', 65, taCenter);
    AddColumn('Failures', 60, taCenter);
    AddColumn('Errors', 60, taCenter);
    AddColumn('Warnings', 60, taCenter);
    AddColumn('Test Time', 99, taCenter);
    AddColumn('Total Time', 99, taCenter);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 2;
    RowSelect := False;
    ScrollbarStyle := ssNone;
    ShowHeader := False;
    TabOrder := 5;
    Focusable := False;
    OnDrawCell  := @DrawGridCell;
  end;

  Splitter2 := TfpgSplitter.Create(ClientArea);
  with Splitter2 do
  begin
    Name := 'Splitter2';
    SetPosition(2, 396, 537, 7);
    Align := alTop;
    AutoSnap := False;
  end;

  meErrorMessage := TfpgMemo.Create(ClientArea);
  with meErrorMessage do
  begin
    Name := 'meErrorMessage';
    SetPosition(2, 403, 537, 98);
    Align := alClient;
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
    MinHeight := 80;
  end;

  FailureGrid := TfpgStringGrid.Create(Bevel5);
  with FailureGrid do
  begin
    Name := 'FailureGrid';
    SetPosition(0, 84, 533, 106);
    Anchors := [anLeft,anRight,anTop];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('Test Name', 125, taLeftJustify);
    AddColumn('Type', 100, taLeftJustify);
    AddColumn('Message', 288, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 6;
    Options := [go_SmoothScroll];
    OnRowChange  := @FailureGridRowChanged;
  end;

  {@VFD_BODY_END: GUITestRunner}
  {%endregion}

  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;
  MainMenu.AddMenuItem('Test Tree', nil).SubMenu := mnuTestTree;
  MainMenu.AddMenuItem('Options', nil).SubMenu := mnuOptions;
  MainMenu.AddMenuItem('Actions', nil).SubMenu := mnuActions;
  MainMenu.AddMenuItem('Help', nil).SubMenu := mnuHelp;

  InitCommands;
end;

procedure TGUITestRunner.AutoSaveActionExecute(Sender: TObject);
begin
  with miAutoSaveConfiguration do
  begin
    Checked := not Checked;
    if not Checked then
      { only save the checked state }
      gINI.WriteBool(cnConfigIniSection, 'AutoSave', miAutoSaveConfiguration.Checked);
  end;
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.HideTestNodesOnOpenClicked(Sender: TObject);
begin
  with miHideTestNodesOnOpen do
    Checked := not Checked;
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.ShowTestedNodeClicked(Sender: TObject);
begin
  with miShowTestedNode do
    Checked := not Checked;
  AutoSaveConfiguration;
end;

procedure TGUITestRunner.EnableWarningsActionExecute(Sender: TObject);
begin
  if FHoldOptions then
    Exit;
  with miEnableWarnings do
    Checked := not Checked;
end;

procedure TGUITestRunner.FailureGridRowChanged(Sender: TObject; ARow: Integer);
begin
  if ARow <> -1 then
  begin
    TestTree.Selection := TFailureDataObject(FailureGrid.Objects[0, ARow]).TreeNode;
    TestTree.Invalidate;
  end;

  if ARow = -1 then
    ClearFailureMessage
  else
    DisplayFailureMessage(ARow);
end;

procedure TGUITestRunner.ClearFailureGrid;
var
  i: integer;
begin
  for i := 0 to FailureGrid.RowCount-1 do
    TFailureDataObject(FailureGrid.Objects[0,i]).Free;
  FailureGrid.RowCount := 0;
end;

procedure TGUITestRunner.miHelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui();
end;

procedure TGUITestRunner.miHelpAboutFPTest(Sender: TObject);
begin
  TfpgMessageDialog.Information('About FPTest', 'Created by Graeme Geldenhuys - 2016');
end;

{ TBaseCommand }

constructor TBaseCommand.Create(AForm: TGUITestRunner);
begin
  inherited Create;
  FForm := AForm;
end;

{ TExitCommand }

procedure TExitCommand.Execute;
begin
  FForm.Close;
end;

{ TSelectAllCommand }

procedure TSelectAllCommand.Execute;
begin
  with FForm do
  begin
    ApplyToTests(TestTree.RootNode.FirstSubNode, @EnableTest);
    UpdateStatus(True);
  end;
end;

{ TRunAllSelectedCommand }

procedure TRunAllSelectedCommand.Execute;
begin
  with FForm do
  begin
    if Suite = nil then
      Exit;

    HoldOptions(True);
    try
      SetUp;
      RunTheTest(Suite);
    finally
      HoldOptions(False);
    end;
  end;
end;

{ TRunCurrentSelectionCommand }

procedure TRunCurrentSelectionCommand.Execute;
begin
  with FForm do
  begin
    SetUp;
    ListSelectedTests;
    ProgressBar.MaxValue := 1;
    ScoreBar.MaxValue := 1;
    HoldOptions(True);
    try
      RunTheTest(Suite);
    finally
      HoldOptions(False);
      FreeAndNil(FSelectedTests);
    end;
  end;
end;

{ TDeselectAllCommand }

procedure TDeselectAllCommand.Execute;
begin
  with FForm do
  begin
    ApplyToTests(TestTree.RootNode.FirstSubNode, @DisableTest);
    UpdateStatus(True);
  end;
end;

{ TSelectFailedCommand }

procedure TSelectFailedCommand.Execute;
var
  i: Integer;
  ANode: TfpgTreeNode;
begin
  with FForm do
  begin
    { deselect all }
    ApplyToTests(TestTree.RootNode.FirstSubNode, @DisableTest);
    { select failed }
    for i := 0 to FailureGrid.RowCount-1 do
    begin
      ANode := TFailureDataObject(FailureGrid.Objects[0, i]).TreeNode;
      SetNodeState(ANode, true);
    end;
    UpdateStatus(True);
  end;
end;


{ TStopTestsCommand }

procedure TStopTestsCommand.Execute;
begin
  with FForm do
  begin
    Suite.HaltTesting;
  end;
end;

{ THideTestNodesOnOpenCommand }

procedure THideTestNodesOnOpenCommand.Execute;
var
  ANode: TfpgTreeNode;
begin
  with FForm do
  begin
    if TestTree.RootNode.Count = 0 then
      Exit;

    TestTree.BeginUpdate;
    try
      TestTree.FullExpand; // initially set everything to expanded
      ANode := TestTree.RootNode.FirstSubNode;
      if ANode <> nil then
      begin
        ANode.Expand;
        CollapseNonGrandparentNodes(ANode);
        MakeNodeVisible(ANode);
      end;
    finally
      TestTree.EndUpdate;
    end;
  end;
end;

{ TExpandAllNodesCommand }

procedure TExpandAllNodesCommand.Execute;
begin
  with FForm do
  begin
    TestTree.FullExpand;
    if (TestTree.Selection <> nil) then
      MakeNodeVisible(TestTree.Selection)
    else if(TestTree.RootNode.Count > 0) then
      TestTree.Selection := TestTree.RootNode.FirstSubNode;
  end;
end;

{ TDeselectCurrentCommand }

procedure TDeselectCurrentCommand.Execute;
begin
  with FForm do
  begin
    ApplyToTests(TestTree.Selection, @DisableTest);
    SetNodeState(TestTree.Selection, False);
    UpdateStatus(True);
  end;
end;

{ TSelectCurrentCommand }

procedure TSelectCurrentCommand.Execute;
begin
  with FForm do
  begin
    ApplyToTests(TestTree.Selection, @EnableTest);
    SetNodeState(TestTree.Selection, True);
    UpdateStatus(True);
  end;
end;


end.
