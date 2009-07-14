{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit dunit2; 

interface

uses
    TestFrameworkIfaces, ProjectsManagerIface, TestListenerIface, 
  TestFrameworkProxyIfaces, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('dunit2', @Register); 
end.
