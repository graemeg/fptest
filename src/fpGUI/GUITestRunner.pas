unit GUITestRunner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_label, fpg_menu,
  fpg_progressbar, fpg_grid, fpg_tree, fpg_imagelist, fpg_command_intf,
  fpg_panel,
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
    TestTree: TfpgTreeView;
    mnuFile: TfpgPopupMenu;
    mnuTestTree: TfpgPopupMenu;
    mnuOptions: TfpgPopupMenu;
    mnuActions: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    {@VFD_HEAD_END: GUITestRunner}
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
    procedure AddSuccess(Test: ITestProxy);
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
    procedure SwitchNodeState(ANode: TfpgTreeNode);
    procedure UpdateTestTreeState;
    procedure SetNodeState(ANode: TfpgTreeNode; AEnabled: boolean);
    procedure UpdateNodeImage(ANode: TfpgTreeNode);
    procedure UpdateNodeState(ANode: TfpgTreeNode);
    procedure RefreshTestCount;
    procedure SaveConfiguration;
    procedure LoadConfiguration;
    function HasParent(ANode: TfpgTreeNode): Boolean;
    procedure SetupGUINodes;
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


  TExitCommand = class(TInterfacedObject, ICommand)
  private
    FForm: TGUITestRunner;
  public
    constructor Create(AForm: TGUITestRunner); reintroduce;
    procedure   Execute;
  end;


  TSelectAllCommand = class(TInterfacedObject, ICommand)
  private
    FForm: TGUITestRunner;
  public
    constructor Create(AForm: TGUITestRunner); reintroduce;
    procedure Execute;
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

procedure TGUITestRunner.AddSuccess(Test: ITestProxy);
begin

end;

procedure TGUITestRunner.AddError(Error: TTestFailure);
begin

end;

procedure TGUITestRunner.AddFailure(Failure: TTestFailure);
begin

end;

procedure TGUITestRunner.AddWarning(AWarning: TTestFailure);
begin

end;

procedure TGUITestRunner.TestingStarts;
begin

end;

procedure TGUITestRunner.StartTest(Test: ITestProxy);
begin

end;

procedure TGUITestRunner.EndTest(Test: ITestProxy);
begin

end;

procedure TGUITestRunner.TestingEnds(TestResult: TTestResult);
begin

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
//  AutoSaveConfiguration;
  FreeAndNil(FTests); // Note this is an object full of Interface refs
  Suite := nil;       // Take down the test proxys
//  ClearRegistry;      // Take down the Registered tests
end;

procedure TGUITestRunner.FormShow(Sender: TObject);
begin
  TestTree.FullExpand;
  SetupGUINodes;
  // TODO: graeme
//  ResultsView.Columns[8].Width := ResultsView.Columns[8].Width;
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

function TGUITestRunner.get_TestResult: TTestResult;
begin

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
end;

function TGUITestRunner.EnableTest(Test: ITestProxy): boolean;
begin

end;

function TGUITestRunner.DisableTest(Test: ITestProxy): boolean;
begin

end;

procedure TGUITestRunner.ApplyToTests(root: TfpgTreeNode; const func: TTestFunc);
begin

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

procedure TGUITestRunner.SwitchNodeState(ANode: TfpgTreeNode);
begin
   Assert(ANode <> nil);
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
  Assert(Assigned(ANode));
  Test := NodeToTest(ANode);
  Assert(Assigned(Test));

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
  SetPosition(428, 213, 548, 542);
  WindowTitle := 'GUI Test Runner';
  Hint := '';
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
    SetPosition(0, 24, 548, 28);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
  end;

  TestTree := TfpgTreeView.Create(self);
  with TestTree do
  begin
    Name := 'TestTree';
    SetPosition(4, 56, 540, 296);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 8;
    ShowImages := True;
    ImageList := FImageList;
    StateImageList := FStateImageList;
    OnStateImageClicked  := @ProcessClickOnStateIcon;
    OnKeyPress  := @ProcessKeyPressOnTreeview;
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(396, 36, 120, 20);
    AddMenuItem('Quit', 'Ctrl+Q', nil);
  end;

  mnuTestTree := TfpgPopupMenu.Create(self);
  with mnuTestTree do
  begin
    Name := 'mnuTestTree';
    SetPosition(396, 60, 120, 20);
    miSelectAll := AddMenuItem('Select All', 'Ctrl+Alt+A', nil);
  end;

  mnuOptions := TfpgPopupMenu.Create(self);
  with mnuOptions do
  begin
    Name := 'mnuOptions';
    SetPosition(396, 84, 120, 20);
  end;

  mnuActions := TfpgPopupMenu.Create(self);
  with mnuActions do
  begin
    Name := 'mnuActions';
    SetPosition(396, 108, 120, 20);
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(396, 132, 120, 20);
  end;

  {@VFD_BODY_END: GUITestRunner}
  {%endregion}

  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;

  InitCommands;
end;

{ TExitCommand }

constructor TExitCommand.Create(AForm: TGUITestRunner);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TExitCommand.Execute;
begin
  FForm.Close;
end;

{ TSelectAllCommand }

constructor TSelectAllCommand.Create(AForm: TGUITestRunner);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TSelectAllCommand.Execute;
begin
  with FForm do
  begin
    ApplyToTests(TestTree.RootNode, @EnableTest);
    UpdateStatus(True);
  end;
end;

end.
