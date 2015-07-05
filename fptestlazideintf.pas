{ Copyright (C) 2004 Vincent Snijders

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  
  Abstract:
    This unit adds a new project type and a new unit type to the IDE.
    New Project Type:
      FPTest Application - A Free Pascal program for FPTest tests.
      
    New Unit Type:
      FPTest test - A unit with a unit test.

  Adapted from FPCUnit package.
}
unit FPTestLazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms, FPTestTestCaseOpts;

type
  { TFPTestApplicationDescriptor }

  TFPTestApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;
  
  { TFPTestConsoleApplicationDescriptor }

  TFPTestConsoleApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

  { TFileDescPascalUnitFPTestTestCase }

  TFileDescPascalUnitFPTestTestCase = class(TFileDescPascalUnit)
  private
    FTestCaseName: string;
    FCreateSetup: boolean;
    FCreateTearDown: boolean;
  public
    constructor Create; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceSource(const {%H-}Filename, {%H-}SourceName,
                                {%H-}ResourceName: string): string;override;
    function GetImplementationSource(const {%H-}Filename, {%H-}SourceName,
                                     {%H-}ResourceName: string): string; override;
    property TestCaseName: string read FTestCaseName write FTestCaseName;
    property CreateSetup: boolean read FCreateSetup write FCreateSetup;
    property CreateTeardown: boolean read FCreateTeardown write FCreateTeardown;
  end;

var
  ProjectDescriptorFPTestApplication: TFPTestApplicationDescriptor;
  ProjectDescriptorFPTestConsoleApp: TFPTestConsoleApplicationDescriptor;
  FileDescriptorFPTestTestCase: TFileDescPascalUnitFPTestTestCase;

procedure Register;

implementation

uses
  strutils;

resourcestring
  sFPTestTestApp = 'FPTest Test Application';
  sFPTestTestAppDesc = 'FPTest Test Application%sAn application to run '
    +'FPTest test cases.%sThe application source is automatically maintained by '
    +'Lazarus.';
  sFPTestTestCase = 'FPTest Test Case';
  sFPTestTestCaseDesc = 'FPTest Test Case%sA unit containing a FPTest Test '
    +'Case.';
  sWriteYourOwnTest = 'Write your own test';
  sFPTestConsoleTestApp = 'FPTest Console Test Application';
  sFPTestConsoleTestDesc = 'FPTest Console Test Application%sAn application '
    +'to run FPTest test cases in console mode.%sThe application source is '
    +'automatically maintained by Lazarus.';

procedure Register;
begin
  FileDescriptorFPTestTestCase:=TFileDescPascalUnitFPTestTestCase.Create;
  RegisterProjectFileDescriptor(FileDescriptorFPTestTestCase);
  ProjectDescriptorFPTestConsoleApp := TFPTestConsoleApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFPTestConsoleApp);
  ProjectDescriptorFPTestApplication:=TFPTestApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorFPTestApplication);
end;

{ TFPTestApplicationDescriptor }

constructor TFPTestApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FPTest Application';
end;

function TFPTestApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=sFPTestTestApp;
end;

function TFPTestApplicationDescriptor.GetLocalizedDescription: string;
var
  le: string;
begin
  le := System.LineEnding;
  Result:=Format(sFPTestTestAppDesc,[le+le,le]);
end;

function TFPTestApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fptestproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.LoadDefaultIcon;

  // create program source
  le:=LineEnding;
  NewSource:='program FPTestProject1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  Interfaces, Forms, GUITestRunner;'+le
    +le
    +'begin'+le
    +'  Application.Initialize;'+le
    +'  RunRegisteredTests;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('LCL');
  AProject.AddPackageDependency('fptest_lcl');
  
  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
  Result:=mrOK;
end;

function TFPTestApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPTestTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:=mrOK;
end;

{ TFileDescPascalUnitFPTestTestCase }

constructor TFileDescPascalUnitFPTestTestCase.Create;
begin
  inherited Create;
  Name:='FPTest TestCase';
  DefaultFilename:='testcase.pas';
  DefaultSourceName:='TestCase1';
end;

function TFileDescPascalUnitFPTestTestCase.CreateSource(const Filename,
  SourceName, ResourceName: string): string;
var
  LE: string;
begin
  CreateSetup := false;
  CreateTeardown := false;
  LE:=LineEnding;
  with TFPTestTestCaseOptionsForm.Create(nil) do
  try
    edDefaultName.Text := 'T' + SourceName;
    ShowModal;
    if edDefaultName.Text <> '' then
      TestCaseName := edDefaultName.Text
    else
      TestCaseName:= 'T' + SourceName;
    if cbSetup.Checked then
      CreateSetup := True
    else
      CreateSetup := False;
    if cbTeardown.Checked then
      CreateTeardown := True
    else
      CreateTeardown := False;
  finally
    Free;
  end;

  Result:=
     'unit '+SourceName+';'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+GetInterfaceUsesSection+';'+LE
    +LE
    +GetInterfaceSource(Filename,SourceName,ResourceName)
    +'implementation'+LE
    +LE
    +GetImplementationSource(Filename,SourceName,ResourceName)
    +'end.'+LE
    +LE;
end;

function TFileDescPascalUnitFPTestTestCase.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', TestFramework';
end;

function TFileDescPascalUnitFPTestTestCase.GetLocalizedName: string;
begin
  Result:=sFPTestTestCase;
end;

function TFileDescPascalUnitFPTestTestCase.GetLocalizedDescription: string;
begin
  Result:=Format(sFPTestTestCaseDesc,[#13]);
end;

function TFileDescPascalUnitFPTestTestCase.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
var
  le: string;
  setupMethod: string;
  teardownMethod: string;
  protectedSection: string;
begin
  le:=System.LineEnding;
  if CreateSetup or CreateTeardown then
    protectedSection := '  protected' + le;
  if CreateSetup then
    setupMethod := '    procedure SetUp; override;' + le;
  if CreateTeardown then
    teardownMethod := '    procedure TearDown; override;' + le;
  Result := 'type' + le
    + le
    +'  '+TestCaseName+'= class(TTestCase)'+le
    + protectedSection
    + setupMethod
    + teardownMethod
    +'  published'+le
    +'    procedure TestHookUp;'+le
    +'  end;'+le+le;
end;

function TFileDescPascalUnitFPTestTestCase.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  le: string;
  setupMethod: string;
  teardownMethod: string;
begin
  le:=System.LineEnding;
  if CreateSetup then
  setupMethod :=  'procedure '+TestCaseName+'.SetUp;'+le
                  +'begin'+le
                  +le
                  +'end;'+le;
  if CreateTeardown then
  teardownMethod := 'procedure '+TestCaseName+'.TearDown;'+le
                   +'begin'+le
                   +le
                   +'end;'+le;
  Result:='procedure '+TestCaseName+'.TestHookUp;'+le
    +'begin'+le
    +'  Fail('+QuotedStr(sWriteYourOwnTest)+');'+le
    +'end;'+le
    +le
    + IfThen(CreateSetup, setupMethod + le)
    + IfThen(CreateTeardown, teardownMethod + le)
    +'initialization'+le
    +'  RegisterTest('+TestCaseName+'.Suite);'
    +le;
end;

{ TFPTestConsoleApplicationDescriptor }

constructor TFPTestConsoleApplicationDescriptor.Create;
begin
  inherited Create;
  Name:='FPTest Console Application';
end;

function TFPTestConsoleApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=sFPTestConsoleTestApp;
end;

function TFPTestConsoleApplicationDescriptor.GetLocalizedDescription: string;
var
  le: string;
begin
  le := System.LineEnding;
  Result:=Format(sFPTestConsoleTestDesc,[le+le,le]);
end;

function TFPTestConsoleApplicationDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  NewSource: string;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('fptestproject1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  NewSource := 'program FPTestProject1;' + LineEnding
    + LineEnding
    + '{$mode objfpc}{$H+}' + LineEnding
    + LineEnding
    + 'uses' + LineEnding
    + '  Classes, TextTestRunner;' + LineEnding
    + LineEnding
    + 'begin' + LineEnding
    + '  RunRegisteredTests;' + LineEnding
    + 'end.' + LineEnding
  ;

  AProject.MainFile.SetSourceText(NewSource);

  // add FCL dependency
  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('fptest');

  // compiler options
  AProject.LazCompilerOptions.UseLineInfoUnit:=true;
  Result:=mrOK;
end;

function TFPTestConsoleApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  LazarusIDE.DoNewEditorFile(FileDescriptorFPTestTestCase,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  Result:=mrOK;
end;

end.

