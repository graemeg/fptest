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
//  LoadConfiguration;
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
  result := fpgGetAppConfigDir(False)  + TEST_INI_FILE;
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

  ARootNode.StateImageIndex := 0;
  ARootNode.ImageIndex := 0;
  // TODO: Graeme must review if this is the same as the commented line
  ARootNode.Data := Pointer(ATest);
//  ARootNode.Data := TObject(FTests.Add(ATest));

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
  FStateImageList.AddImage(fpgImages.GetImage('usr.state1'));  // checked
  FStateImageList.AddImage(fpgImages.GetImage('usr.state2'));  // checked disabled
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
