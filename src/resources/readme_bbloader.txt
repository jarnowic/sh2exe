You can use these tools to create and test scripts prior to create the executable in sh2exe, it is the same environment. It is also portable, so you can place on a flash drive and have your minishell to use everywere.

The "home" directory under this path is shared by both versions of Busybox, Cygwin creates a soft link pointing to it. Create your stuff inside it.

These executables do not accept parameters, they are just used to start a bash session.

Unless you know what you are doing, do not edit the files under /etc of cygwin directory, these files defines an environment like the sh2exe uses.