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

  bbloader.lpr - Loads an unpacked busybox on certain path maintaining the
  sh2exe standards.

  This program is very similar to scrloader.lpr but is not so focused in a
  minimal file size.

  The executable will load Cygwin or Win32 MinGW builds based on its own name:

  busybox.win32.exe - Will load the Win32 MinGW build
  busybox.cygwin.exe - Loads the Cygwin build. }

program bbloader;

{$mode objfpc}{$H+}

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, scrloader_tools;

const
  Version = '0.1.0';

begin
  SetConsoleWindowPosition;
  SetEnvironment;

  if ExtractFileName(Paramstr(0)) = 'busybox.win32.exe' then begin
    SetEnvironmentVariable('HOME', PChar(ExtractFilePath(Paramstr(0)) + '\home'));
    // init.sh is stored under Cygwin dir to make easy to load it, but is the
    // same file for MinGW build.
    ValidateRunProcess('builds\win32\busybox.exe bash -c builds/cygwin/etc/init.sh');
  end
  else if ExtractFileName(Paramstr(0)) = 'busybox.cygwin.exe' then begin
    ValidateRunProcess('builds\cygwin\bin\busybox.exe bash -l -c /etc/init.sh')
  end
  else begin
    writeln('Help-me! I am a loader program who do not know what to do!' + #13#10 +
      'I must be called "busybox.win32.exe" or "busybox.cygwin.exe"'  + #13#10 +
      'I think you ought to know I am feeling very depressed.');
    readln;
  end;

end.



