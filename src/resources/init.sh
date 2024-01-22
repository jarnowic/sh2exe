#!/bin/bash

EN=$(uname)
if [ ${EN:0:6} = "CYGWIN" ]; then
  S2E_EXENAME="$(cygpath "$S2E_EXENAME")"
  S2E_STARTDIR="$(cygpath "$S2E_STARTDIR")"
  rm /busybox.exe
  S2E_BUSYBOX="Cygwin"
else
  S2E_BUSYBOX="Win32"
fi

export S2E_EXENAME S2E_STARTDIR S2E_BUSYBOX

unzip $HOME/stuff.zip -q -d $HOME
rm $HOME/stuff.zip

cd $HOME
sh main.sh "$@"