Welcome to DUnit2
=================

This is a re-write of DUnit with a few different design goals in mind.
The original code was created by the late Peter McNab. He single
handedly added some excellent features to DUnit2 and used the extensive
test suite from tiOPF as his playground.

As of July 2009, I (Graeme Geldenhuys) decided to continue Peter's work
on DUnit2. I applied some of my own ideas listed below:

 * No need for Delphi.NET support. The product doesn't exist anymore
   and .NET has its own testing framework anyway, called NUnit.
 * Don't need prior to Delphi 7 support
 * Must have Free Pascal Compiler (FPC) support.
 * With FPC support comes the idea that it must be cross-platform
   friendly as well.
 * Hence the next item - removing the idea of writing to the Windows
   Registry. INI or XML config files to a great job, are easy to edit
   and works for all OS platforms.


In both DUnit and DUnit2, unit tests comprise classes derived from
TTestCase, each containing one or more published test procedures as
shown in the example below. TTestCase is now an interfaced object.

type
  TTestMyComms = class(TTestCase)
  published
    procedure VerifyThatThereIsAnUnAssignedCommPort;
    procedure VerifyThatTheCommPortOpens;
    procedure VerifyThatTheCommPortCloses;
  end;

Through the magic of RTTI DUnit2 is able to execute the published test
procedures in an orderly fashion. Code written into test methods
performs tests on user's code and calls one or more Check() procedures
to signal pass or fail to the test framework.

For more information about DUnit2, have a look at the various documents
in the 'docs' directory.


