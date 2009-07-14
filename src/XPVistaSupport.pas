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
*)
unit XPVistaSupport;

interface
uses
{$IFDEF CLR}
  System.Reflection,
  System.Text,
{$ENDIF}
  Classes;

function  LocalAppDataPath : string;

{==============================================================================}
implementation
uses
  Windows,
  {$IFNDEF VER130}
  SHFolder,
  {$ENDIF}
  SysUtils;

{$IFDEF VER130}
  function LocalAppDataPath : string;
  begin
    Result := ExtractFilePath(ParamStr(0)) + '\';
  end;
{$ELSE}
{$IFNDEF XPVISTA}
  function LocalAppDataPath : string;
  begin
    Result := ExtractFilePath(ParamStr(0)) + '\';
  end;
{$ELSE}
  {$IFDEF CLR} //DotNet
    function LocalAppDataPath : string;
      const
        SHGFP_TYPE_CURRENT = 0;
      var
        Path: StringBuilder;
    begin
      Path := StringBuilder.Create;
      Path.Capacity := MAX_PATH;
      SHGetFolderPath(0,                                            //hwndOwner
                     CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE,      //int Folder
                     0,                                             //hToken
                     SHGFP_TYPE_CURRENT,                            //dwFlags
                     path);                                         //pszPath
      Result := string(Path.ToString) + '\' + ChangeFileExt(ExtractFileName(ParamStr(0)), '\');
      Path := nil;
      if not DirectoryExists(Result) then
      try
        CreateDir(Result);
      except
        raise Exception.Create('Unable to create application INI directory at: ' + Result);
      end;
    end;
  {$ELSE}  //Win32
    function LocalAppDataPath : string;
      const
        SHGFP_TYPE_CURRENT = 0;
      var
        path: array [0..MAX_PATH] of char;
    begin
      SHGetFolderPath(0,                                            //hwndOwner
                     CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE,      //int Folder
                     0,                                             //hToken
                     SHGFP_TYPE_CURRENT,                            //dwFlags
                     @path[0]);                                     //pszPath
      Result := string(Path) + '\' + ChangeFileExt(ExtractFileName(ParamStr(0)), '\');
      if not DirectoryExists(Result) then
      try
        CreateDir(Result);
      except
        raise Exception.Create('Unable to create application INI directory at: ' + Result);
      end;
    end;
  {$ENDIF}
{$ENDIF}
{$ENDIF}

initialization

end.
