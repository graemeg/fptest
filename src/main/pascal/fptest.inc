{#(@)$Id: DUnit.inc 53 2010-05-12 07:08:46Z jarrodh $ }
{  DUnit: An XTreme testing framework for Delphi programs. }
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2008.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 * Peter McNab <mcnabp@gmail.com>
 *
 *******************************************************************************
*)

{$I jedi.inc} // Pull in the JCL/J-VCL shared directives

//------------------------------------------------------------------------------
// User-configurable options
//------------------------------------------------------------------------------
{$DEFINE XMLLISTENER}
// Stack traces. Zero or one can be defined.
// Include a stack trace using JCL
{$DEFINE JCL_STACK_TRACE}
// Include a stack trace using madExcept
{.$DEFINE MADEXCEPT_STACK_TRACE}
// FastMM memory manager
{$DEFINE FASTMM}

//------------------------------------------------------------------------------
// Auto-configuration
//------------------------------------------------------------------------------
// Stack trace reporting
{$IFDEF MADEXCEPT_STACK_TRACE}
  {$IFDEF JCL_STACK_TRACE}
    {$MESSAGE FATAL 'Only MADEXCEPT_STACK_TRACE or JCL_STACK_TRACE can be defined, not both.'}
  {$ELSE}
    {$DEFINE REPORT_STACK_TRACE}
  {$ENDIF JCL_STACK_TRACE}
{$ENDIF MADEXCEPT_STACK_TRACE}
{$IFDEF JCL_STACK_TRACE}
  {$DEFINE REPORT_STACK_TRACE}
{$ENDIF JCL_STACK_TRACE}

// XML Listener
{$IFDEF XMLLISTENER}
  // Use either xdom or OpenXML depending on Delphi version
  {$IFNDEF XDOM}
    {$IFNDEF DKADOMCORE}
      {$IFNDEF DELPHI2010_UP}
        {$DEFINE XDOM}
      {$ELSE}
        {$DEFINE DKADOMCORE}
      {$ENDIF DELPHI2010_UP}
    {$ENDIF DKADOMCORE}
  {$ELSE}
    {$IFDEF DKADOMCORE}
      {$MESSAGE FATAL 'Only XDOM or DKADOMCORE can be defined, not both.'}
    {$ENDIF}
  {$ENDIF XDOM}
{$ENDIF}

// CLX support
{$IFDEF LINUX}
  {$DEFINE DUNIT_CLX}
{$ENDIF}

// .NET support
{$IFDEF CLR}
  {$UNDEF FASTMM}
{$ENDIF CLR}

// Compiler options

{$IFDEF CLR}
  {$UNSAFECODE ON}
{$ENDIF CLR}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$IFDEF DELPHI2009_UP}
    {$WARN EXPLICIT_STRING_CAST OFF}
  {$ENDIF DELPHI2009_UP}
{$ENDIF DELPHI7_UP}

