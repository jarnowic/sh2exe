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
  
  ufrmMain.pas - The main form

  For more information, open uTsh2exe.pas }

unit ufrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Menus, ExtCtrls, Buttons, uTsh2exe, ufrmAbout,  ufrmHelp;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnExe: TButton;
    ImageList1: TImageList;
    edtExe: TLabeledEdit;
    imgCygwin: TImage;
    imgMinGW: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbFileList: TListBox;
    MainMenu: TMainMenu;
    dlgOpen: TOpenDialog;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    mnuExtractBusybox: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelpTopics: TMenuItem;
    mnuHelp: TMenuItem;
    mnuGenerateSample: TMenuItem;
    mnuCreateExe: TMenuItem;
    mnuCreate: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuExit: TMenuItem;
    mnuCleanList: TMenuItem;
    mnuRemoveSelected: TMenuItem;
    mnuAddFile: TMenuItem;
    mnuFile: TMenuItem;
    rbCygwin: TRadioButton;
    rbMinGW: TRadioButton;
    dlgSave: TSaveDialog;
    dlgDirectory: TSelectDirectoryDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure btnExeClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuAddFileClick(Sender: TObject);
    procedure mnuCleanListClick(Sender: TObject);
    procedure mnuCreateExeClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuExtractBusyboxClick(Sender: TObject);
    procedure mnuGenerateSampleClick(Sender: TObject);
    procedure mnuHelpTopicsClick(Sender: TObject);
    procedure mnuRemoveSelectedClick(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}
{$R sh2exe_stuff.rc}

procedure TfrmMain.mnuAddFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    lbFileList.Items.AddStrings(dlgOpen.Files);
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmMain.btnExeClick(Sender: TObject);
begin
  if dlgSave.Execute then
    edtExe.Text := dlgSave.Filename;
end;

procedure TfrmMain.mnuCleanListClick(Sender: TObject);
begin
  if MessageDlg('Clear file list?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    lbFileList.Clear;
end;

procedure TfrmMain.mnuCreateExeClick(Sender: TObject);
var
  sh2exe: Tsh2exe;
begin
  if not DirectoryExists(ExtractFileDir(edtExe.Text)) then begin
    ShowMessage('Invalid Path');
    exit;
  end;

  sh2exe := Tsh2exe.Create(rbMinGW.Checked);
  sh2exe.GenerateNewExe(edtExe.Text, lbFileList.Items);
  sh2exe.Free;
end;


procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.mnuExtractBusyboxClick(Sender: TObject);
var
  sh2exe: Tsh2exe;
begin
  if dlgDirectory.Execute then begin
    sh2exe := Tsh2exe.Create(True);
    sh2exe.ExtractBusybox(dlgDirectory.FileName);
    sh2exe.Free;
  end;
end;

procedure TfrmMain.mnuGenerateSampleClick(Sender: TObject);
var
  sh2exe: Tsh2exe;
begin
  sh2exe := Tsh2exe.Create(rbMinGW.Checked);
  sh2exe.GenerateSample;
  sh2exe.Free;
end;

procedure TfrmMain.mnuHelpTopicsClick(Sender: TObject);
begin
  Application.CreateForm(TfrmHelp, frmHelp);
  frmHelp.ShowModal;
  frmHelp.Free;
end;

procedure TfrmMain.mnuRemoveSelectedClick(Sender: TObject);
var
  i: Integer;
begin
  if lbFileList.SelCount > 0 then
    for i:=lbFileList.Items.Count - 1 downto 0 do
      if lbFileList.Selected[i] then
        lbFileList.Items.Delete(i);
end;

end.

