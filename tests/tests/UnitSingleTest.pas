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

unit UnitSingleTest;
interface
uses
  TestFrameworkIfaces,
//  TestModules,
  TestFramework;

type
  TSingleTest = class(TTestCase)
  published
    procedure SingleTestDLL;
  end;

implementation
uses
  SysUtils;

procedure TSingleTest.SingleTestDLL;
begin
  Check(True);
end;

initialization
  RegisterTest(TSingleTest.Suite);
end.
