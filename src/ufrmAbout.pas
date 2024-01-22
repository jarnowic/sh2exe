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

  ufrmAbout.pas - Just the about box }

unit ufrmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, resource, versiontypes, versionresource;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblVersion: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.FormCreate(Sender: TObject);

var
  Stream: TResourceStream;
  vr: TVersionResource;
  fi: TVersionFixedInfo;
begin
  Stream:= TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
  try
    vr:= TVersionResource.Create;
    try
      vr.SetCustomRawDataStream(Stream);
      fi:= vr.FixedInfo;
      lblVersion.Caption := 'v. ' + IntToStr(fi.FileVersion[0]) + '.' +
        IntToStr(fi.FileVersion[1]) + '.' + IntToStr(fi.FileVersion[2]);
      vr.SetCustomRawDataStream(nil)
    finally
      vr.Free;
    end;
  finally
    Stream.Free;
  end
end;

end.

