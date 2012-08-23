SUBDIRS = src
libdir = $(prefix)/erlyssh/lib
lib_LTLIBRARIES = libreadlinedrv.la
libreadlinedrv_la_SOURCES = $(top_srcdir)/native/linux/readline_drv.c
INCLUDES = -I $(ERLANG_ROOT_DIR)/usr/include
libreadlinedrv_la_LIBADD = -lreadline -lhistory
bin_PROGRAMS = essh

essh:   
	echo "#!/bin/sh\n" > essh
	echo "erl -noinput -pa $(prefix)/erlyssh/ebin -run essh start $(libdir) \$${ESSH_LIST_HOME}/\$$* " >>essh
