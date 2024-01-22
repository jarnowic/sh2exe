{***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Shell Script To Executable.
  02-2016 - Rodrigo Morette  

  scrloader_tools.pas - Utilities based on Windows API and misc functions.

  This is file is designed to generate a code smaller as possible, so only
  Windows unit is allowed here }

unit scrloader_tools;

{$mode objfpc}{$H+}

interface

uses
  Windows;


const // Borrowed from ShellAPI
  FO_DELETE                = $0003;
  FOF_SILENT               = $0004;
  FOF_NOCONFIRMATION       = $0010;
  FOF_NOCONFIRMMKDIR       = $0200;
  FOF_NOERRORUI            = $0400;

type // Borrowed from ShellAPI
SHFILEOPSTRUCT = Record
  wnd               : HWND;
  wFunc             : UINT;
  pFrom             : LPCSTR;
  pTo               : LPCSTR;
  fFlags            : FILEOP_FLAGS;
  fAnyOperationsAborted : BOOL;
  hNameMappings     : LPVOID;
  lpszProgressTitle : LPCSTR;
end;


function SetEnvironment: String;
function ReplaceBackslashes(s: String): String;
function BackslashSpace(s: string): string;
function CreateTmpDir: String;
function ValidadePath(Path: string): boolean;
function Deltree(const Path: String): Integer;
function RunProcess(Command: string): boolean;
function GetEnvironmentVar(const VarName: string): string;
function ExtractResource(Resource, FilePath: String): Boolean;
procedure SetConsoleWindowPosition;
function GetConsoleWindow: HWND; stdcall; external kernel32 name 'GetConsoleWindow';
procedure ValidateRunProcess(Command: String);


implementation

function SetEnvironment: String;
var
  s :string;
  ArrayStr: array [0..MAX_PATH] of char;
  i: integer;
begin
  // Current directory is assigned to s
  GetCurrentDirectory(MAX_PATH, ArrayStr);
  SetString(s, PChar(@ArrayStr[0]), Length(ArrayStr));

  // Environment variables that will be inherited by the shell
  SetEnvironmentVariable('S2E_STARTDIR', PChar(ReplaceBackslashes(s)));
  SetEnvironmentVariable('S2E_EXENAME', PChar(ReplaceBackslashes(ParamStr(0))));

  // Join parameters in one string and fixes backslashes and spaces
  Result := '';
  for i := 1 to Paramcount do begin
    s := BackslashSpace(ReplaceBackslashes(ParamStr(i)));
    Result := Result + s + ' ';
  end;

end;

// Searches for spaces in a string and replace to "\ "
// This is necessary to pass parameters to bash
function BackslashSpace(s: string): string;
var
  i, j: integer;
  ArrayChar: array of char;
begin

  // Count the number of spaces in the string
  j := 0;
  for i := 1 to length(s) do
    if s[i] = ' ' then j := j + 1;

  // If no spaces then get out of here
  if j = 0 then begin
    result := s;
    exit;
  end;

  // Set the lenght of the array based on size of s adding one additional
  // position for every space found
  SetLength(ArrayChar,Length(s) + j);

  j := 0; // j controls the array
  for i := 1 to length(s) do begin // i controls the string
    if s[i] = ' ' then begin
      ArrayChar[j] := '\';
      // Shifts j one position ahead of i, as the array receives 2 chars
      j := j + 1;
      ArrayChar[j] := ' ';
    end else
      ArrayChar[j] := s [i];
    j := j + 1;
  end;

  // Write the array to result
  SetString(result, PChar(@ArrayChar[0]), Length(ArrayChar));

end;


// Replaces any backslash found in an string by a forward slash
function ReplaceBackslashes(s: String): String;
var
  i: integer;
begin
   for i := 1 to length(s) do
    if s[i] = '\' then
      s[i] := '/';
    result := s;
end;

// Create a temporary directory containing a pseudo random number on its name.
// This avoids conflicts with another instances.
function CreateTmpDir(): String;
var
  TmpDir, s: String;
begin
  randomize;
  str(random(30000), s);
  TmpDir := GetEnvironmentVar('TMP') + '\busyboxtmp' + s;
  CreateDirectory(PChar(TmpDir),nil);
  Result := TmpDir;
end;

// Validate if a path exists or not
function ValidadePath(Path: string): boolean;
var
  Attr: integer;
begin
  Attr := GetFileAttributesA(PCHAR(Path));
  if Attr <> -1 then
    Result := True
  else
    Result := False;
end;

// Recursively delete a directory
function Deltree(const Path: String): Integer;
var
  FileOpStruct : TShFileOpStruct;
begin
  if ValidadePath(Path) then begin
    FillChar(FileOpStruct, SizeOf(FileOpStruct), #0);
    FileOpStruct.wFunc := FO_DELETE;
    FileOpStruct.pFrom := PChar(Path + #0#0);
    FileOpStruct.pTo := nil;
    FileOpStruct.fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_NOERRORUI
      or FOF_NOCONFIRMMKDIR;
    FileOpStruct.lpszProgressTitle := nil;
    Result := ShFileOperation(FileOpStruct);
  end;
end;

// Start a process using CreateProcess and wait for it ends.
// Returns TRUE if successful or FALSE if the process creation fails
function RunProcess(Command: string): boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  FillChar(ProcessInfo, SizeOf(TProcessInformation), #0);
  StartupInfo.cb := SizeOf(StartupInfo);

  if CreateProcess(nil, @Command[1], nil, nil, False, CREATE_NEW_PROCESS_GROUP,
    nil, nil, StartupInfo, ProcessInfo) then begin
    // Wait for the process ends
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    Result := True;
  end
  else begin
    Result := False;
    exit;
  end;

  // Closes process handles
  if ProcessInfo.hProcess <> 0 then
    CloseHandle(ProcessInfo.hProcess);
  if ProcessInfo.hThread <> 0 then
    CloseHandle(ProcessInfo.hThread);
end;


// Get the value of an environment variable
function GetEnvironmentVar(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  // Get required buffer size (inc. terminal #0)
  BufSize := Windows.GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    Windows.GetEnvironmentVariable(PChar(VarName),
      PChar(Result), BufSize);
  end
  else
    // No such environment variable
    Result := '';
end;

// A member of codecall.net called Luthfi posted this very nice code to read and
// unpack resources just using Windows API. This is very handyful as
// TResourceStream increases a lot the executable size
function ExtractResource(Resource, FilePath: String): Boolean;
var
  ResHandle: THandle;
  HGlobal : THandle;
  Data : Pointer;
  ResSize : DWORD;
  FileHandle : THandle;
  Count : DWORD;
begin
  Result := False;
  // get the handle to the resource we are interested in
  ResHandle := FindResource(HInstance, PChar(Resource), RT_RCDATA);
  if ResHandle = 0 then Exit;

  // load the resource to memory
  HGlobal := LoadResource(HInstance, ResHandle);
  if HGlobal = 0 then Exit;

  // find the address of the resource we've just load into memory
  Data := LockResource(HGlobal);
  // get the size of the resource
  ResSize := SizeOfResource(HInstance, ResHandle);

  // create the temporary file
  FileHandle := CreateFile(PChar(FilePath)
  , GENERIC_WRITE, FILE_SHARE_READ
  , nil
  , CREATE_ALWAYS
  , FILE_ATTRIBUTE_ARCHIVE
  , 0);

  try
    // write the resource into the file
    WriteFile(FileHandle, Data^, ResSize, Count, nil);
  finally
    // close the file (and do the actual storing to disk
    CloseHandle(FileHandle);
    Result := True;
  end;
end;

// Place the console on the center of the desktop
// Found this in stackoverflow, interesting procedure.
procedure SetConsoleWindowPosition;
var
  ConsoleHwnd: HWND;
  R: TRect;
begin
  ConsoleHwnd := GetConsoleWindow;
  // Center the console window
  GetWindowRect(ConsoleHwnd, R);
  SetWindowPos(ConsoleHwnd, 0,
    (GetSystemMetrics(SM_CXVIRTUALSCREEN) - (R.Right - R.Left)) div 2,
    (GetSystemMetrics(SM_CYVIRTUALSCREEN) - (R.Bottom - R.Top)) div 2,
    0, 0, SWP_NOSIZE);
end;

procedure ValidateRunProcess(Command: String);
begin
  if not RunProcess(Command) then begin
    writeln('Process creation error');
    readln;
  end;
end;



end.
