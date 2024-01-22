This small program packs a shell script and Busybox into a single EXE, so the script can run on any Windows machine, even if it doesn’t have a bash shell. It is possible to include another files along with the script, so this can be used to deploy files, for example. This is an initial version, probably has a lot of bugs.

This is NOT a compiler neither encrypter, if you need something like this you can take a look at SHC Shell Compiler.

The program supports two versions of Busybox, the port to Win32/MinGW from frippery.org or the Cygwin version. Cygwin version is more powerful, but the final executable is too big (1.45MB), so the Win32 version is preferred, as it generates an executable of 460KB. Seems huge for a script, but keep in mind that an entire shell is packed together. Do not use UPX on this, it triggers malware alert in some anti-virus software.

It is possible also to unpack the embedded Busybox versions to a directory and use this environment to create/test scripts prior to pack them into an executable.

It is written in Lazarus and released under GPLv2

I'm not sure if all anti-virus accepts a program of this nature, as the scripts operates in %TMP%. I made some tests, tried to run some scripts and download things with wget, didn’t trigger the AV alarm in following environments:
Win7 + Symantec Endpoint Protection
Win10 + Kasperky Internet Security
WinXP + AVG Free Edition
WinXP + Avast Free Edition

List of commands available on each Busybox version:

Win32:
. : [ [[ alias ar ash awk base64 basename bash bbconfig break bunzip2 bzcat bzip2 cal cat catv cd chdir chmod cksum clear cmp comm command continue cp cpio cut date dc dd df diff dirname dos2unix dpkg-deb du echo ed egrep env eval exec exit expand export expr false fgrep find fold ftpget ftpput getopt getopts grep groups gunzip gzip hash hd head help hexdump history id ipcalc kill killall less let ln local logname ls lzcat lzma lzop lzopcat man md5sum mkdir mktemp mv nc od patch pgrep pidof printenv printf ps pwd read readonly return rev rm rmdir rpm2cpio sed seq set sh sha1sum sha256sum sha3sum sha512sum shift shuf sleep sort source split stat strings sum tac tail tar tee test times touch tr trap true truncate type ulimit umask unalias uname uncompress unexpand uniq unix2dos unlink unlzma unlzop unset unxz unzip usleep uudecode uuencode vi wait wc wget which whoami xargs xz xzcat yes zcat

Cygwin:
. : [ [[ add-shell alias ar ash awk base64 basename bash bbconfig bg break bunzip2 busybox bzcat bzip2 cal cat catv cd chat chdir chgrp chmod chown chpst chroot cksum clear cmp comm command continue cp cpio crond crontab cryptpw cttyhack cut cygpath date dc dd df diff dirname dnsd dnsdomainname dos2unix du echo ed egrep env envdir envuidgid eval exec exit expand export expr fakeidentd false fg fgrep find flock fold free fsync ftpd ftpget ftpput fuser getopt getopts grep groups gunzip gzip hash hd head help hexdump history hostid hostname httpd hush id inetd install ipcalc ipcrm jobs kill killall killall5 less let ln local logger login logname logread lpd lpq lpr ls lsof lzcat lzma lzop lzopcat makedevs makemime man md5sum mesg microcom mkdir mkfifo mknod mktemp more mountpoint mpstat mt mv nc nice nmeter nohup nslookup od patch pgrep pidof ping ping6 pipe_progress pkill popmaildir printenv printf ps pscan pstree pwd pwdx read readlink readonly realpath reformime remove-shell renice reset resize return rm rmdir rpm rpm2cpio run-parts runsv runsvdir rx script scriptreplay sed sendmail seq set setsid setuidgid sh sha1sum sha256sum sha3sum sha512sum shift shuf sleep smemcap softlimit sort source split start-stop-daemon stat strings stty sum sv svlogd sync syslogd tac tail tar tcpsvd tee telnet telnetd test tftp tftpd time timeout times top touch tr traceroute trap true tty ttysize type udpsvd ulimit umask unalias uname uncompress unexpand uniq unix2dos unlink unlzma unlzop unset unxz unzip uptime usleep uudecode uuencode vi volname wait watch wc wget which whoami whois xargs xz xzcat yes zcat