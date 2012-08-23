#erlsrcdir = $(prefix)/src
#erlsrc_DATA =   essh.erl \
#                essh_agent.erl \
#                essh_scheduler.erl

erlbeamdir = $(prefix)/erlyssh/ebin
erlbeam_DATA =  essh.beam \
                essh_agent.beam \
                essh_scheduler.beam

CLEANFILES = $(erlbeam_DATA)
SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
