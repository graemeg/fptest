unit GUITestRunner;

{$mode objfpc}{$H+}

{.$define DEBUG}
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
{$ENDIF}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_label, fpg_menu,
  fpg_progressbar, fpg_grid, fpg_tree, fpg_imagelist, fpg_command_intf,
  fpg_panel, fpg_button, fpg_splitter, fpg_gauge, fpg_memo, fpg_basegrid,
  fpg_scrollbar,
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
    Button3: TfpgButton;
    Button4: TfpgButton;
    Button5: TfpgButton;
    Bevel2: TfpgBevel;
    btnRunSelected: TfpgButton;
    btnRunCurrent: TfpgButton;
    Button8: TfpgButton;
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
    ScoreBar: TfpgGauge;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    ResultsView: TfpgStringGrid;
    Splitter2: TfpgSplitter;
    Memo1: TfpgMemo;
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
    FTestResult:    TTestResult;
    FRunning:       Boolean;
    FTests:         TInterfaceList;
    FSelectedTests: TInterfaceList;
    FTotalTime:     Int64;
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
    miSelectAll: TfpgMenuItem;

    { implement the IStatusListeners interface }
    procedure Status(const ATest: ITestProxy; AMessage: string);

    { implement the ITestListener interface }
    procedure AddSuccess(ATest: ITestProxy);
    procedure AddError(Error: TTestFailure);
    procedure AddFailure(Failure: TTestFailure);
    procedure AddWarning(AWarning: TTestFailure);
    procedure TestingStarts;
    procedure StartTest(Test: ITestProxy);
    procedure EndTest(Test: ITestProxy);
    procedure TestingEnds(TestResult: TTestResult);
    function  ShouldRunTest(const ATest :ITestProxy):Boolean;

    { implement the ITestListenerX interface }
    procedure StartSuite(Suite: ITestProxy);
    procedure EndSuite(Suite: ITestProxy);

    procedure CommandHandler(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ProcessClickOnStateIcon(Sender: TObject; ANode: TfpgTreeNode);
    procedure ProcessKeyPressOnTreeview(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure DrawGridCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
    procedure SetSuite(const AValue: ITestProxy);
    function get_TestResult: TTestResult;
    procedure set_TestResult(const AValue: TTestResult);
    procedure LoadSuiteConfiguration;
    procedure EnableUI(AEnable: Boolean);
    function  IniFileName: TfpgString;
    procedure InitCommands;
    function  EnableTest(Test: ITestProxy): boolean;
    function  DisableTest(Test: ITestProxy): boolean;
    procedure ApplyToTests(root: TfpgTreeNode; const func: TTestFunc);
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
    function HasParent(ANode: TfpgTreeNode): Boolean;
    procedure SetupGUINodes;
    procedure MakeNodeVisible(Node: TfpgTreeNode);
    procedure SetTreeNodeImage(Node: TfpgTreeNode; imgIndex: Integer);
    procedure ClearStatusMessage;
    procedure RunTheTest(ATest: ITestProxy);
    procedure HoldOptions(const AValue: boolean);
    procedure ClearResult;
    procedure ClearFailureMessage;
  protected
    procedure InitTree; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  published
    { The Test Suite to be run in this runner }
    property Suite: ITestProxy read FSuite write SetSuite;
    { The result of the last Test run }
    property TestResult : TTestResult read get_TestResult write set_TestResult;
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


  TRunSelectedCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;


  TRunCurrentCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;

  TDeselectAllCommand = class(TBaseCommand)
  public
    procedure Execute; override;
  end;

procedure RunRegisteredTests;


implementation

uses
  fpg_utils,
  dbugintf,
  TestFrameworkProxy,
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

procedure TGUITestRunner.Status(const ATest: ITestProxy; AMessage: string);
begin

end;

procedure TGUITestRunner.AddSuccess(ATest: ITestProxy);
var
  LOverridesGUI: Boolean;
  LHasRunTimePropsSet: Boolean;
begin
  SendDebug('success: ' + ATest.Name);
  assert(assigned(ATest));
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

procedure TGUITestRunner.AddError(Error: TTestFailure);
//var
//  ListItem: TListItem;
begin
  SendDebug('error: ' + Error.FailedTest.Name);
  FTestFailed := True;
  // TODO: graemeg
  //ListItem := AddFailureItem(Failure);
  //ListItem.ImageIndex := imgERROR;
  //TProgressBarCrack(ScoreBar).Color := clERROR;
  //TProgressBarCrack(ScoreBar).RecreateWnd;
  SetTreeNodeImage(TestToNode(Error.FailedTest), imgERROR);
  UpdateStatus(True);
end;

procedure TGUITestRunner.AddFailure(Failure: TTestFailure);
//var
//  ListItem: TListItem;
begin
  SendDebug('failure: ' + Failure.FailedTest.Name);
  FTestFailed := True;
  // TODO: graemeg
  //ListItem := AddFailureItem(Failure);
  //ListItem.ImageIndex := imgFAILED;
  //if TestResult.ErrorCount = 0 then //Dont override higher priority error colour
  //begin
  //  TProgressBarCrack(ScoreBar).Color := clFAILURE;
  //  TProgressBarCrack(ScoreBar).RecreateWnd;
  //end;
  SetTreeNodeImage(TestToNode(Failure.failedTest), imgFAILED);
  UpdateStatus(True);
end;

procedure TGUITestRunner.AddWarning(AWarning: TTestFailure);
begin
  SendDebug('warning: ' + AWarning.FailedTest.Name);
end;

procedure TGUITestRunner.TestingStarts;
begin
  SendDebug('*** Testing Starts ****');
  FTotalTime := 0;
  UpdateStatus(True);
//  TProgressBarCrack(ScoreBar).Color := clOK;
//  TProgressBarCrack(ScoreBar).RecreateWnd;
  ClearStatusMessage;
end;

procedure TGUITestRunner.StartTest(Test: ITestProxy);
var
  Node: TfpgTreeNode;
begin
  assert(assigned(TestResult));
  assert(assigned(Test));
  Node := TestToNode(Test);
  assert(assigned(Node));
  SetTreeNodeImage(Node, imgRUNNING);
  // TODO: graeme
//  if ShowTestedNodeAction.Checked then
//  begin
    MakeNodeVisible(Node);
    TestTree.Invalidate;
//  end;
//  ErrorMessageRTF.Lines.Clear;
  UpdateStatus(False);
end;

procedure TGUITestRunner.EndTest(Test: ITestProxy);
begin

end;

procedure TGUITestRunner.TestingEnds(TestResult: TTestResult);
begin
  SendSeparator;
end;

function TGUITestRunner.ShouldRunTest(const ATest: ITestProxy): Boolean;
begin

end;

procedure TGUITestRunner.StartSuite(Suite: ITestProxy);
begin

end;

procedure TGUITestRunner.EndSuite(Suite: ITestProxy);
begin

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
  TestTree.ImageList := nil;
  for i := FImageList.Count-1 downto 0 do
    FImageList[i].Image := nil; // clear the references
  FImageList.Free;

  TestTree.StateImageList := nil;
  for i := FStateImageList.Count-1 downto 0 do
    FStateImageList[i].Image := nil;  // clear the references
  FStateImageList.Free;

//  ClearResult;
  AutoSaveConfiguration;
  FreeAndNil(FTests); // Note this is an object full of Interface refs
  Suite := nil;       // Take down the test proxys
//  ClearRegistry;      // Take down the Registered tests
end;

procedure TGUITestRunner.FormShow(Sender: TObject);
begin
  ResultsView.Cells[0, 0] := 'Tests';
  ResultsView.Cells[1, 0] := 'Run';
  ResultsView.Cells[2, 0] := 'Failures';
  ResultsView.Cells[3, 0] := 'Errors';
  ResultsView.Cells[4, 0] := 'Warnings';
  ResultsView.Cells[5, 0] := 'Test Time';
  ResultsView.Cells[6, 0] := 'Total Time';

  TestTree.FullExpand;
  SetupGUINodes;
  // TODO: graeme
//  ResultsView.Columns[8].Width := ResultsView.Columns[8].Width;
  TestTree.SetFocus;
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

function TGUITestRunner.get_TestResult: TTestResult;
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
    EnableUI(False)
end;

procedure TGUITestRunner.set_TestResult(const AValue: TTestResult);
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
  SendDebug('IniFileName = ' + Result);
end;

procedure TGUITestRunner.InitCommands;
begin
  // instantiate the Command classes
//  btnAdd.SetCommand(TAddCommand.Create(memName1));
//  btnQuit.SetCommand(TExitCommand.Create);

  // The menu item File|Quit shares the command of btnQuit
  mnuFile.MenuItemByName('Quit').SetCommand(TExitCommand.Create(self));

  SelectAllCommand := TExitCommand.Create(self);
  miSelectAll.SetCommand(TSelectAllCommand.Create(self));

  btnRunSelected.SetCommand(TRunSelectedCommand.Create(self));
  btnRunCurrent.SetCommand(TRunCurrentCommand.Create(self));
  btnSelectAll.SetCommand(TSelectAllCommand.Create(self));
  btnSelectNone.SetCommand(TDeselectAllCommand.Create(self));
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

procedure TGUITestRunner.UpdateStatus(const FullUpdate: Boolean);
begin

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
  // TODO: graemeg
(*
  FailureListView.Items.Clear;
  ResetProgress;
  Update;

  with ResultsView.Items[0] do
  begin
    SubItems[0] := '';    //Test Count
    SubItems[1] := '';    //Tests Run
    SubItems[2] := '';    //Failures
    SubItems[3] := '';    //Errors
    SubItems[4] := '';    //Warnings
    SubItems[5] := '';    //Test's Time
    SubItems[6] := '';    //Total Test Time

    if Suite <> nil then
    begin
      TotalTestsCount := Suite.countEnabledTestCases;
      SubItems[0] := IntToStr(TotalTestsCount);
      ProgressBar.Max := TotalTestsCount;
    end
    else
      ProgressBar.Max:= 10000;

    ScoreBar.Max := ProgressBar.Max;
  end;
*)
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
  end;
end;

procedure TGUITestRunner.SwitchNodeState(ANode: TfpgTreeNode);
begin
  {$IFDEF DEBUG}
  Assert(ANode <> nil);
  {$ENDIF}
   SetNodeState(ANode, not NodeToTest(ANode).Enabled);
end;

procedure TGUITestRunner.UpdateTestTreeState;
var
  Node: TfpgTreeNode;
begin
  if TestTree.RootNode.Count > 0 then
  begin
    try
      Node := TestTree.RootNode.FirstSubNode;
      while Node <> nil do
      begin
        UpdateNodeState(Node);
        Node := TestTree.NextNode(Node);
      end
    finally
      TestTree.Invalidate;
    end;
  end;
end;

procedure TGUITestRunner.SetNodeState(ANode: TfpgTreeNode; AEnabled: boolean);
var
  MostSeniorChanged: TfpgTreeNode;
  n: TfpgTreeNode;
begin
   Assert(ANode <> nil);

   if (NodeToTest(ANode).Enabled <> AEnabled) then
     NodeToTest(ANode).Enabled := AEnabled;

   MostSeniorChanged := ANode;
   if AEnabled then
   begin
     n := ANode;
     while HasParent(n) do
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
  Test :ITestProxy;
begin
  Test := NodeToTest(ANode);
  if not Test.Enabled then
  begin
    ANode.StateImageIndex := imgDISABLED;
  end
  else if HasParent(ANode) and (ANode.Parent.StateImageIndex <= imgPARENT_DISABLED) then
  begin
    ANode.StateImageIndex := imgPARENT_DISABLED;
  end
  else
  begin
    ANode.StateImageIndex := imgENABLED;
  end;

  if HasParent(ANode) and (ANode.Parent.StateImageIndex >= imgEXCLUDED) then
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
  {$IFDEF DEBUG}
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
  // TODO: graeme
{
  if Assigned(Suite) then
    ResultsView.Items[0].SubItems[0] := IntToStr(TotalTestsCount)
  else
    ResultsView.Items[0].SubItems[0] := '';
}
end;

procedure TGUITestRunner.AutoSaveConfiguration;
begin
  // TODO: graeme
//  if AutoSaveAction.Checked then
    SaveConfiguration;
end;

procedure TGUITestRunner.SaveConfiguration;
var
  i: Integer;
begin
  if Suite <> nil then
    Suite.SaveConfiguration(IniFileName, False {UseRegistryAction.Checked}, True);
  // TODO: graeme
(*
  SaveFormPlacement;
  SaveRegistryAction;

  with GetIniFile(IniFileName) do
  try
    { center splitter location }
    WriteInteger(cnConfigIniSection, 'ResultsPanel.Height',
      ResultsPanel.Height);

    { error box }
    WriteInteger(cnConfigIniSection, 'ErrorMessage.Height',
      ErrorBoxPanel.Height);
    WriteBool(cnConfigIniSection, 'ErrorMessage.Visible',
      ErrorBoxVisibleAction.Checked);

    { failure list configuration }
    with FailureListView do begin
      for i := 0 to Columns.Count-1 do
      begin
       WriteInteger(cnConfigIniSection,
                     Format('FailureList.ColumnWidth[%d]', [i]),
                     Columns[i].Width);
      end;
    end;

    { other options }
    WriteBool(cnConfigIniSection, 'HideTestNodesOnOpen',      HideTestNodesOnOpenAction.Checked);
    WriteBool(cnConfigIniSection, 'BreakOnFailures',          BreakOnFailuresAction.Checked);
    WriteBool(cnConfigIniSection, 'ReportMemoryLeakTypes',    ReportMemoryLeakTypeOnShutdownAction.Checked);
    WriteBool(cnConfigIniSection, 'SelectTestedNode',         ShowTestedNodeAction.Checked);
    WriteBool(cnConfigIniSection, 'ShowRunTimeProperties',    ShowTestCasesWithRunTimePropertiesAction.Checked);
    WriteBool(cnConfigIniSection, 'ShowOverriddenFailures',   ShowOverriddenFailuresAction.Checked);
    WriteBool(cnConfigIniSection, 'ShowEarlyExitTests',       ShowEarlyExitedTestAction.Checked);

    WriteInteger(cnConfigIniSection, 'PopupX',                FPopupX);
    WriteInteger(cnConfigIniSection, 'PopupY',                FPopupY);

    // Settings common to all test runners
    WriteBool(cnRunners, 'FailOnNoChecksExecuted',    FailIfNoChecksExecutedAction.Checked);
    WriteBool(cnRunners, 'FailOnMemoryLeaked',        FailTestCaseIfMemoryLeakedAction.Checked);
    WriteBool(cnRunners, 'EnableWarnings',            EnableWarningsAction.Checked);
    WriteBool(cnRunners, 'IgnoreSetUpTearDownLeaks',  IgnoreMemoryLeakInSetUpTearDownAction.Checked);
    WriteBool(cnRunners, 'InhibitSummaryLevelChecks', InhibitSummaryLevelChecksAction.Checked);
  finally
    Free;
  end;
*)
end;

procedure TGUITestRunner.LoadConfiguration;
begin

end;

function TGUITestRunner.HasParent(ANode: TfpgTreeNode): Boolean;
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
//  TestTree.NextVisualNode()
end;

procedure TGUITestRunner.SetTreeNodeImage(Node: TfpgTreeNode; imgIndex: Integer);
var
  n: TfpgTreeNode;
begin
  n := Node;
  while n <> nil do
  begin
    if imgIndex > n.ImageIndex then
       n.ImageIndex    := imgIndex;
    if imgIndex = imgRUNNING then
      n := nil
    else
      n := n.Parent;
  end;
end;

procedure TGUITestRunner.ClearStatusMessage;
begin
//  ErrorMessageRTF.Lines.Clear;
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
        TXMLListener.Create(LocalAppDataPath + Suite.Name
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
  if FTestResult <> nil then
  begin
    FTestResult := nil;
    ClearFailureMessage;
  end;
end;

procedure TGUITestRunner.ClearFailureMessage;
begin

end;

procedure TGUITestRunner.InitTree;
begin
  FTests.Clear;
  FillTestTree(Suite);
  SetUp;
{ TODO: graeme
  if HideTestNodesOnOpenAction.Checked then
    HideTestNodesAction.Execute
  else
    ExpandAllNodesAction.Execute;
}
  TestTree.Selection := TestTree.RootNode.FirstSubNode;
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
  SetPosition(507, 209, 548, 542);
  WindowTitle := 'GUI Test Runner';
  Hint := '';
  ShowHint := True;
  MinWidth := 540;
  MinHeight := 450;
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;
  OnShow := @FormShow;

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
    Flat := True;
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
    Flat := True;
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

  Button3 := TfpgButton.Create(Toolbar);
  with Button3 do
  begin
    Name := 'Button3';
    SetPosition(64, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 4;
    Focusable := False;
  end;

  Button4 := TfpgButton.Create(Toolbar);
  with Button4 do
  begin
    Name := 'Button4';
    SetPosition(88, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 5;
    Focusable := False;
  end;

  Button5 := TfpgButton.Create(Toolbar);
  with Button5 do
  begin
    Name := 'Button5';
    SetPosition(112, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    TabOrder := 6;
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

  btnRunSelected := TfpgButton.Create(Toolbar);
  with btnRunSelected do
  begin
    Name := 'btnRunSelected';
    SetPosition(148, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'usr.starttest';
    TabOrder := 8;
    Focusable := False;
  end;

  btnRunCurrent := TfpgButton.Create(Toolbar);
  with btnRunCurrent do
  begin
    Name := 'btnRunCurrent';
    SetPosition(172, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'usr.startselectedtest';
    TabOrder := 9;
    Focusable := False;
  end;

  Button8 := TfpgButton.Create(Toolbar);
  with Button8 do
  begin
    Name := 'Button8';
    SetPosition(196, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
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
    Flat := True;
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
    Flat := True;
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
    Flat := True;
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
    Flat := True;
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
    Flat := True;
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
    Flat := True;
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
    Flat := True;
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
    SetPosition(4, 74, 541, 464);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  bvlTreeAndProgress := TfpgBevel.Create(ClientArea);
  with bvlTreeAndProgress do
  begin
    Name := 'bvlTreeAndProgress';
    SetPosition(2, 2, 537, 350);
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
    AddMenuItem('Quit', 'Ctrl+Q', nil);
  end;

  mnuTestTree := TfpgPopupMenu.Create(self);
  with mnuTestTree do
  begin
    Name := 'mnuTestTree';
    SetPosition(400, 108, 120, 20);
    miSelectAll := AddMenuItem('Select All', 'Ctrl+Alt+A', nil);
  end;

  mnuOptions := TfpgPopupMenu.Create(self);
  with mnuOptions do
  begin
    Name := 'mnuOptions';
    SetPosition(400, 132, 120, 20);
    miAutoSaveConfiguration := AddMenuItem('Auto Save Configuration', '', nil);
    miErrorBoxVisible := AddMenuItem('Error Box Visible', '', nil);
    miAutoChangeFocus := AddMenuItem('Auto Change Focus', '', nil);
    miHideTestNodesOnOpen := AddMenuItem('Hide Test Nodes On Open', '', nil);
    miShowTestedNode := AddMenuItem('Show Tested Node', '', nil);
    miBreakOnFailure := AddMenuItem('Break On Failures', '', nil);
    AddMenuItem('-', '', nil);
    miShowTestCasesWithRuntimeProperties := AddMenuItem('Show TestCases with RunTime Properties', '', nil);
    miShowOverriddenFailures := AddMenuItem('Show Overridden Failures', '', nil);
    miShowExitedEarly := AddMenuItem('Show passing Summary Level Tests which exited early', '', nil);
    AddMenuItem('-', '', nil);
    miFailTestIfNoChecks := AddMenuItem('Fail TestCase if no checks performed', '', nil);
    miEnableWarnings := AddMenuItem('Enable Warnings', '', nil);
    miInhibitSummaryLevelChecks := AddMenuItem('Inhibit Summary Level Checking', '', nil);
  end;

  mnuActions := TfpgPopupMenu.Create(self);
  with mnuActions do
  begin
    Name := 'mnuActions';
    SetPosition(400, 156, 120, 20);
    AddMenuItem('[todo]', '', nil);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(400, 180, 120, 20);
    AddMenuItem('About fpGUI Toolkit', '', nil);
    AddMenuItem('About FPTest...', '', nil);
  end;

  Bevel5 := TfpgBevel.Create(bvlTreeAndProgress);
  with Bevel5 do
  begin
    Name := 'Bevel5';
    SetPosition(2, 265, 533, 83);
    Align := alBottom;
    Hint := '';
    Shape := bsSpacer;
    Style := bsLowered;
  end;

  TestTree := TfpgTreeView.Create(bvlTreeAndProgress);
  with TestTree do
  begin
    Name := 'TestTree';
    SetPosition(2, 2, 533, 263);
    Align := alClient;
    FontDesc := '#Label1';
    Hint := '';
    ShowImages := True;
    TabOrder := 8;
    ImageList := FImageList;
    StateImageList := FStateImageList;
    OnStateImageClicked  := @ProcessClickOnStateIcon;
    OnKeyPress  := @ProcessKeyPressOnTreeview;
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

  ScoreBar := TfpgGauge.Create(Bevel5);
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
    SetPosition(6, 44, 514, 37);
    BackgroundColor := TfpgColor($80000002);
    AddColumn('Tests', 70, taCenter);
    AddColumn('Run', 60, taCenter);
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
    ShowHeader := False;
    TabOrder := 5;
    Focusable := False;
    ScrollBarStyle := ssNone;
    OnDrawCell  := @DrawGridCell;
  end;

  Splitter2 := TfpgSplitter.Create(ClientArea);
  with Splitter2 do
  begin
    Name := 'Splitter2';
    SetPosition(2, 352, 537, 7);
    Align := alTop;
    AutoSnap := False;
  end;

  Memo1 := TfpgMemo.Create(ClientArea);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(2, 359, 537, 103);
    Align := alClient;
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
    MinHeight := 80;
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

{ TRunSelectedCommand }

procedure TRunSelectedCommand.Execute;
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

{ TRunCurrentCommand }

procedure TRunCurrentCommand.Execute;
begin
  with FForm do
  begin
    SetUp;
    ListSelectedTests;
//    ProgressBar.Max := 1;
//    ScoreBar.Max    := 1;
    HoldOptions(True);
    try
      RunTheTest(Suite);
    finally
      HoldOptions(False);
      FSelectedTests.Free;
      FSelectedTests := nil;
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



end.
