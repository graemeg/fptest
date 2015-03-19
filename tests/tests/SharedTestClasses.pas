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

{$IFDEF CLR}
  {$UNSAFECODE ON}
  {$UNDEF FASTMM}
{$ENDIF}

unit SharedTestClasses;
interface
uses
  {$IFDEF CLR}
    System.Reflection,
  {$ENDIF}
  {$IFDEF SELFTEST}
    RefTestFramework,
  {$ELSE}
    TestFramework,
  {$ENDIF}
  {$IFDEF USE_JEDI_JCL}
    JclDebug,
  {$ENDIF}
  SysUtils;

type
  TMiddleOfThreeTestsFail = class(TTestCase)
  published
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test1Passes;
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test2Fails;
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test3Passes;
  end;

  TMiddleOfThreeTestsExcept = class(TTestCase)
  private
    {$IFDEF CLR}
    E: Exception;
    {$ELSE}
    E: SysUtils.Exception;
    {$ENDIF}
  protected
    procedure SetUpOnce; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;
  published
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test1Passes;
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test2Fails;
    {$IFDEF CLR}[Test]{$ENDIF}
     procedure Test3Passes;
  end;

implementation
//uses
//  Windows
  {$IFDEF VER130}
    ,D5Support
  {$ENDIF}
//  ;

{ TMiddleOfThreeTestsFail }

procedure TMiddleOfThreeTestsFail.Test1Passes;
begin
  Check(True);
end;

procedure TMiddleOfThreeTestsFail.Test2Fails;
begin
  Check(False, 'Deliberate failure');
end;

procedure TMiddleOfThreeTestsFail.Test3Passes;
begin
  Check(True);
end;

procedure TMiddleOfThreeTestsExcept.SetUpOnce;
begin
  inherited;
  E := Exception.Create('Deliberate exception');
end;

procedure TMiddleOfThreeTestsExcept.TearDown;
begin
  inherited;
end;

procedure TMiddleOfThreeTestsExcept.TearDownOnce;
begin
  inherited;
  E := nil;
end;

procedure TMiddleOfThreeTestsExcept.Test1Passes;
begin
  Check(True);
end;

procedure TMiddleOfThreeTestsExcept.Test2Fails;
begin
  Check(True);
  raise E;
end;

procedure TMiddleOfThreeTestsExcept.Test3Passes;
begin
  Check(True);
end;

initialization

end.
