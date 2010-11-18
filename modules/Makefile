# coding: utf-8
rep_ebin = ebin

rep_erl = erl

#  $ make NATIVE=true
ifdef NATIVE 
   erlc_params = +native -o $(rep_ebin) $<
else
   erlc_params = -o $(rep_ebin) $<
endif

all: $(rep_ebin)/aybabtu_common.beam \
$(rep_ebin)/aybabtu_urlrewrite.beam \
$(rep_ebin)/aybabtu_download_counter.beam

$(rep_ebin)/regex.beam: $(rep_erl)/regex.erl
	erlc $(erlc_params)

$(rep_ebin)/aybabtu_common.beam: $(rep_erl)/aybabtu_common.erl
	erlc $(erlc_params)

$(rep_ebin)/aybabtu_urlrewrite.beam: $(rep_erl)/aybabtu_urlrewrite.erl
	erlc $(erlc_params)

$(rep_ebin)/aybabtu_download_counter.beam: $(rep_erl)/aybabtu_download_counter.erl
	erlc $(erlc_params)

clean:
	rm ebin/*.beam
