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

  scrloader.lpr - Runs a shell script based on stored resources.

  Resources needed (stored as RCDATA):
  - RES_BUSYBOX : Busybox-w32 compiled with MinGW from http://frippery.org/
  - RES_INIT    : The init script, it runs prior to user's defined.
  - RES_STUFF   : A zip file containing the scripts and support files defined
                  by the user. The program will search for a main.sh to run.

  The RES_BUSYBOX and RES_STUFF resources should not be loaded on the EXE at the
  compilation time of this program, the sh2exe program will manage this. This
  program itself is a loaded as a resource in sh2exe.

  The main objective here to keep this program smaller as possible, so only the
  Windows unit is used, all is being done thougt Windows API. Currently,
  Lazarus generates an EXE with size of 42KB, not bad. I don't want to use
  UPX, it causes too many problems with anti-virus programs.

  Besides that, this loader supports Cygwin's Busybox, but the executable size
  increases a lot (the final result is something around 1.45MB). It is stored
  as RAR self extract archive as a resource called RES_BBCYG instead of
  RES_BUSYBOX. }

program scrloader;

{$mode objfpc}{$H+}

{$APPTYPE CONSOLE}

{$R scrloader.rc}

uses
  Windows, scrloader_tools;

const
  Version = '0.1.0';

var
 TmpDir: string;

// Make sure to delete the TMP directory if console window is closed
function ConsoleEvent(CtrlType: LongWord):LongBool; stdcall;
begin
  if (CtrlType = CTRL_CLOSE_EVENT) then
  begin
    Sleep(2000); //Gives time to Cygwin close completely, it blocks the tmp dir.
    Deltree(TmpDir);
  end;
  Result := True;
end;

var
  Parameters: String;
begin
  SetConsoleWindowPosition;
  TmpDir := CreateTmpDir;
  SetConsoleCtrlHandler(@ConsoleEvent, True);

  try
    Parameters := SetEnvironment;

    // Extracts the Busybox resource to TmpDir
    if ExtractResource('RES_BUSYBOX', TmpDir + '\busybox.exe') then begin
      //Win32 MinGW Busybox environment
      SetEnvironmentVariable('HOME', PChar(TmpDir));
      ExtractResource('RES_INIT', TmpDir + '\init.sh');
      ExtractResource('RES_STUFF', TmpDir + '\stuff.zip');
      ValidateRunProcess(TmpDir + '\busybox.exe bash -c "$HOME/init.sh ' +
        Parameters + '"');
    end
    // Win32 MinGW Busybox resource not found, attempt to start a Cygwin
    // environment
    else if ExtractResource('RES_BBCYG', TmpDir + '\busybox.exe') then begin
      ValidateRunProcess(TmpDir + '\busybox.exe -s -d"' + TmpDir + '"' );
      ExtractResource('RES_INIT', TmpDir + '\etc\init.sh');
      ExtractResource('RES_STUFF', TmpDir + '\home\stuff.zip');
      ValidateRunProcess(TmpDir + '\bin\busybox.exe bash -l -c "/etc/init.sh ' +
        Parameters + '"');
    end
    else begin
      writeln('ERROR: Mandatory Busybox resource not found!');
      readln;
    end;
  finally
    Deltree(TmpDir);
  end;

end.


