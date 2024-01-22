#!/bin/bash

# This file is used by both builds of busybox, it is placed here to make 
# easy to Cygwin locate it.
#
# Under the EXE loader, thats how this file is executed:
# Win32:  builds\win32\busybox.exe bash -c builds/cygwin/etc/init.sh
# Cygwin: builds\cygwin\bin\busybox.exe bash -l -c /etc/init.sh


EN=$(uname)
if [ ${EN:0:6} = "CYGWIN" ]; then
  S2E_EXENAME="$(cygpath "$S2E_EXENAME")"
  S2E_STARTDIR="$(cygpath "$S2E_STARTDIR")"
  S2E_BUSYBOX="Cygwin"
  rm -rf /home &> /dev/null
  ln -s "$S2E_STARTDIR/home" "/home"
  echo "Cygwin Busybox build"
  echo -e "http://www.cygwin.com\n"
else
  S2E_BUSYBOX="Win32"
  echo Win32 MinGW Busybox build
  echo -e "http://frippery.org/busybox/\n"
fi

PS1="[$S2E_BUSYBOX \W]\$ "

export PS1 S2E_EXENAME S2E_STARTDIR S2E_BUSYBOX

cd "$HOME"
bash
