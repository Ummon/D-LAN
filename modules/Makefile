# coding: utf-8
rep_ebin = ebin

rep_erl = erl

#  $ make NATIVE=true
ifdef NATIVE 
   erlc_params = +native -o $(rep_ebin) $<
else
   erlc_params = -o $(rep_ebin) $<
endif

all: $(rep_ebin) \
$(rep_ebin)/d_lan_common.beam \
$(rep_ebin)/d_lan_urlrewrite.beam \
$(rep_ebin)/d_lan_download_counter.beam \
$(rep_ebin)/d_lan_lang.beam

$(rep_ebin): FORCE
	[ -d $(rep_ebin) ] || mkdir $(rep_ebin)
   
FORCE:

$(rep_ebin)/d_lan_common.beam: $(rep_erl)/d_lan_common.erl
	erlc $(erlc_params)

$(rep_ebin)/d_lan_urlrewrite.beam: $(rep_erl)/d_lan_urlrewrite.erl
	erlc $(erlc_params)

$(rep_ebin)/d_lan_download_counter.beam: $(rep_erl)/d_lan_download_counter.erl
	erlc $(erlc_params)
   
$(rep_ebin)/d_lan_lang.beam: $(rep_erl)/d_lan_lang.erl
	erlc $(erlc_params)


clean:
	rm ebin/*.beam
