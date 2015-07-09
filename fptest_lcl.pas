{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fptest_lcl;

interface

uses
  GUITestRunner, FPTestLazIDEIntf, fpteststrtestcaseopts, fptesttestcaseopts, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FPTestLazIDEIntf', @FPTestLazIDEIntf.Register);
end;

initialization
  RegisterPackage('fptest_lcl', @Register);
end.
