{
    This unit only applies to the Free Pascal Compiler.

    Copyright (c) 2009 by Graeme Geldenhuys
       All rights reserved.
}
unit fpchelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GetMethodList(AObject: TObject; AList: TStrings); overload;
procedure GetMethodList(AClass: TClass; AList: TStrings); overload;


implementation


//  Get a list of published methods for a given class or object
procedure GetMethodList(AObject: TObject; AList: TStrings);
begin
  GetMethodList(AObject.ClassType, AList);
end;

// Code copied form objpas.inc:  class function TObject.MethodAddress()
procedure GetMethodList(AClass: TClass; AList: TStrings);
type
  TMethodNameRec = packed record
    name: pshortstring;
    addr: pointer;
  end;

  TMethodNameTable = packed record
    count: dword;
    entries: packed array[0..0] of TMethodNameRec;
  end;

  PMethodNameTable =  ^TMethodNameTable;

var
  methodTable: PMethodNameTable;
  i: dword;
  vmt: TClass;
  idx: integer;
begin
  AList.Clear;
  vmt := aClass;
  while Assigned(vmt) do
  begin
    methodTable := pMethodNameTable((Pointer(vmt) + vmtMethodTable)^);
    if Assigned(MethodTable) then
    begin
      for i := 0 to MethodTable^.count - 1 do
      begin
        idx := AList.IndexOf(MethodTable^.entries[i].name^);
        if (idx <> - 1) then
          //found overridden method so delete it
          AList.Delete(idx);
        AList.AddObject(MethodTable^.entries[i].name^, TObject(MethodTable^.entries[i].addr));
      end;
    end;
    vmt := pClass(pointer(vmt) + vmtParent)^;
  end;
end;


end.

