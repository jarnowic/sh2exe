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

  uTsh2exe.pas - Manages the creation of EXE files with embbeded busybox and
  shell script files.

  This executable stores all necessary stuff as resources in RCDATA:
  RES_BUSYBOX - Win32 MinGW version of Busybox from frippery.org
  RES_BBCYG - Cygwin's Busybox packed as RAR SFX
  RES_SAMPLE - A zip file containg a sample script
  RES_SCRLOADER - The script loader coded in scrloader.lpr
  RES_BBINIT - The init file for extracted busybox in user enviroment
  RES_BBLOADER - The loader file for extracted busybox coded in bbloader.lpr
  RES_READMEBB - A readme file that is included with extracted busybox.

  The operation is very simple:
  To generate the EXE, the program unpack RES_BUSYBOX and RES_SCRLOADER to a temp
  dir and create a zip containing the script and files selected by the user.
  Then it loads RES_SCRLOADER as a PE File and stores RES_BUSYBOX and the zip as
  resources into RES_SCRLOADER, after this saves it as EXE.

  This program don't work properly in x64, needs some adjustments. Also,
  unitPEFile.pas contained in util directory only compiles using FPC 3.0, so
  to build these sources Lazarus 1.6 or above is recomended. }

unit uTsh2exe;

{$mode objfpc}{$M+}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Dialogs, unitPEFile,
  unitResourceDetails, scrloader_tools, KAZip, ufrmWait;

type
  { Tsh2exe }
  Tsh2exe = class
    private
      Win32: Boolean;
      procedure SaveExeFile(StuffPath, ExeName: string);
      function FindDuplicate(FileList: TStrings): string;
      function SearchItem(Item: String; FileList:TStrings): Boolean;
    public
      procedure GenerateSample;
      procedure GenerateNewExe(ExeName: String; FileList: TStrings);
      procedure ExtractBusybox(Path: String);
    published
      Constructor Create(Win32build: Boolean);
  end;


implementation

{ Tsh2exe }

constructor Tsh2exe.Create(Win32build: Boolean);
begin
  Self.Win32 := Win32build;
end;

// Check for selected files with the same name on different directories.
function Tsh2exe.FindDuplicate(FileList: TStrings): string;
var
 i: integer;
 List: TStringList;
begin

  result := '';

  // Extract the file names to a new list
  List := TStringList.Create;
  for i := 0 to FileList.Count - 1 do
    List.Add(ExtractFileName(FileList[i]));

  List.Sort;

  // Search for duplicates, case insensitive
  for i := 0 to List.Count - 2 do
    if AnsiUpperCase(List[i]) = AnsiUpperCase(List[i + 1]) then begin
      result := List.Strings[i];
      exit;
    end;

  List.Free;
end;

function Tsh2exe.SearchItem(Item: String; FileList:TStrings): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to FileList.Count - 1 do
    if ExtractFileName(FileList[i]) = Item then begin
      result := true;
      exit;
    end;
end;

// Generates the executable based on the script_loader.exe (RES_SCRLOADER)
// Expects that the zip file containing RES_STUFF is already in StuffPath
procedure Tsh2exe.SaveExeFile(StuffPath, ExeName: string);
var
  res: TResourceDetails;
  f: TMemoryStream;
  fResourceModule: TResourceModule;
  BusyboxBuild: String;
begin
  StuffPath := StuffPath + '\';

  fResourceModule := TPEResourceModule.Create;
  f := TMemoryStream.Create;

  if Self.Win32 then
    BusyboxBuild := 'RES_BUSYBOX'
  else
    BusyboxBuild := 'RES_BBCYG';

  try

    // Extracts necessary stuff.
    ExtractResource(BusyboxBuild, StuffPath + 'busybox');
    ExtractResource('RES_SCRLOADER', StuffPath + 'scrloader');

    // scrloader is RES_SCRLOADER unpacked
    fResourceModule.LoadFromFile(StuffPath + 'scrloader');

    fResourceModule.SortResources;

    // Writes RES_BUSYBOX into the scrloader
    f.LoadFromFile (StuffPath + 'busybox');
    res := TResourceDetails.CreateResourceDetails (
      fResourceModule, 0, BusyboxBuild, IntToStr (Integer (RT_RCDATA)),
      f.Size, f.Memory);
    fResourceModule.AddResource (res);

    fResourceModule.SortResources;

    // Writes the zip file to scrloader
    f.LoadFromFile (StuffPath + 'stuff');
    res := TResourceDetails.CreateResourceDetails (
      fResourceModule, 0, 'RES_STUFF', IntToStr (Integer (RT_RCDATA)),
      f.Size, f.Memory);
    fResourceModule.AddResource (res);

    fResourceModule.SaveToFile(ExeName);
  finally
    f.Free;
    fResourceModule.Free;
  end;
end;

// Guides the creation of the EXE
procedure Tsh2exe.GenerateNewExe(ExeName: String; FileList: TStrings);
var
  s: string;
  TmpDir: String;
  zip: TKAZip;
begin
    // Check if "main.sh" is between the selected files
  if not SearchItem('main.sh', FileList) then begin
    ShowMessage('Error: "main.sh" not found');
    exit;
  end;

  // Verify if there are files with same names on different directories
  s := FindDuplicate(FileList);
  if s <> '' then begin
    ShowMessage('Found duplicate files named "' + s + '". All these files will ' +
      'be packed on the same directory, please make sure all files have unique names');
    exit;
  end;

  TmpDir := CreateTmpDir;

  try
    Application.CreateForm(TfrmWait, frmWait);
    frmWait.Show;
    frmWait.Repaint;

    zip := TKAZip.Create(nil);
    // Create a zip containing the files selected by the user
    with zip do begin
      StoreRelativePath := False;
      StoreFolders := False;
      CompressionType := ctMaximum;
      CreateZip(TmpDir + '\stuff');
      Open(TmpDir + '\stuff');
      AddFiles(FileList);
      Close;
      zip.free;
    end;

    SaveExeFile(TmpDir, ExeName);

    frmWait.Close;
    frmWait.Free;
  finally
    Deltree(TmpDir);
  end;
end;

procedure Tsh2exe.ExtractBusybox(Path: String);
begin
  CreateDir(Path + '\builds');
  CreateDir(Path + '\builds\win32');
  CreateDir(Path + '\builds\cygwin');
  CreateDir(Path + '\home');
  ExtractResource('RES_BBCYG', Path + '\builds\cygwin\cyg.exe');
  RunProcess(PChar(Path + '\builds\cygwin\cyg.exe -s -d"' + Path +
    '\builds\cygwin\"'));
  ExtractResource('RES_BUSYBOX', Path + '\builds\win32\busybox.exe');
  ExtractResource('RES_BBINIT', Path + '\builds\cygwin\etc\init.sh');
  ExtractResource('RES_BBLOADER', Path + '\busybox.win32.exe');
  ExtractResource('RES_BBLOADER', Path + '\busybox.cygwin.exe');
  ExtractResource('RES_READMEBB', Path + '\readme.txt');
  DeleteFile(Path + '\builds\cygwin\cyg.exe');
  Deltree(Path + '\builds\cygwin\home');
  ShowMessage('Busybox builds extracted to: ' + Path);
end;


procedure Tsh2exe.GenerateSample;
var
  TmpDir: string;
begin
  TmpDir := CreateTmpDir;
  try
    ExtractResource('RES_SAMPLE', TmpDir + '\stuff');
    SaveExeFile(TmpDir, ExtractFilePath(Application.ExeName) + 's2e_sample.exe');
    WinExec(PChar(ExtractFilePath(Application.ExeName) + 's2e_sample.exe'), SW_SHOW);
  finally
    Deltree(TmpDir);
  end;
end;

end.

