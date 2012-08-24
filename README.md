erlyssh - A Parallel SSH Execution Tool
=======

Intro:
--------
This is an Erlang Powered linux command line tool.You can use this tool to 
connect to mass of servers through SSH client simultaneously and parallel 
execute noninteractive shell commands on every connected server. 

Requirements:
--------
1. openssh client with agent foward enabled
2. GNU lib readline(libreadline.so,libhistory.so)
3. Erlang 5.6/OTP R12B or later
4. Linux or Mac OSX
5. autoconf automake libtool required for building

Install:
--------
run autoreconf -i to generate configure Makefile.in etc.

run ./configure

run sudo make install

or you can provide --prefix=/custom-install-path/ as ./configure option

if any buid error occurs, try to check
1. Is libreadline's arch compatible with your erlang runtime
2. Check include path and library path, add your custom -I -L to CFLAGS env
3. Is your default system arch compatible with your erlang runtime, otherwise
add -arch parameter to CFLAGS env

Usage:
--------
###configure server list
export ESSH_LIST_HOME = /your-path-to-put-server-list-files

in ESSH_HOME_DIR put your server list files, Use the following format:

	www-11.internal.org
	www-15.internal.org
	172.19.0.86
	-p1046 172.19.1.87
	#comment: each server address(domain or ip) a line 
	#as ssh command's paras`


###run shell script
Start erlyssh with:

	radiumce@app-88:~$ essh cometd                                             
	-----------------www-11 connected-------------------                    
	-----------------www-15 connected------------------- 

	essh>: 
After 'essh>:'  prompt, you can run any non-interactive commands.
for exmaple:

	radiumce@app-88:~$ essh cometd
	-----------------www-11 connected-------------------
	-----------------www-15 connected-------------------

	essh>: ls
	--------------www-11---------------
	erlang


	[primary server done]
	->>

	essh>:
Input a 'ls' command, and it will parallel executed on www-11,www-15.
In erlyssh the first server on the list will be the primary server. Its output is 
realtime(it is useful when running some long duration commands, such as 'svn up').
when the command on www-15 is done, it will compare with the out put of www-11.
when their output is identical, erlssh just print a '->>' as www-15's output.

	
	essh>: #con 5
	set concurrent execution limits = 5
	essh>: 
Schedule parameter 'con'(limit of parallel connections, default 256) can be set by '#con number'


	essh>: #intv 5
	set execution interval = 5s
	essh>: 
Schedule parameter 'intv'(execution interval, default 0) can be set by '#intv ${seconds}'

	essh>: exit;
	Thanks for using essh, bye.
Use command 'exit;'(ends with ';') to exit the shell.

Tips:
--------
1. erlyssh is interactive(you can use 'cd' to change path) but it can only 
execute non-interactive commands.
2. When there is mass of servers, add

	CheckHostIP no

	StrictHostKeyChecking no

   to your .ssh/config can avoid some yes/no security confirm
3. Many shell commands have their non-interactive mode or corresponded 
non-interactive commands. Such as 'svn' commands have non-interactive
mode and 'sed' can help you to perform plain text processing.


Project Links:
--------
* Homepage: <https://github.com/NetEase/erlyssh>
* Tags: tools, ssh, operation, erlang

How Can I Contribute
--------------------
Fork this project on [GitHub](https://github.com/NetEase/erlyssh), add your improvement, push it to a branch in your fork named for the topic, send a pull request.

You can also file bugs or feature requests under the [issues](https://github.com/NetEase/erlyssh/issues/) page on GitHub.