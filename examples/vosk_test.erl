%% Test vosk
%%
-module(vosk_test).
-export([start/0, start/1]).

%%-define(MODEL,"vosk-model-small-sv-rhasspy-0.15").
-define(MODEL,"vosk-model-small-en-us-0.15").

start() ->
    start("test.wav").

start(WavFilename) ->
    {ok,Fd} = file:open(WavFilename, [raw,read,binary]),
    _ = file:read(Fd, 48),  %% skip header
    M = vosk:model_new(filename:join(code:priv_dir(vosk),?MODEL)),
    R = vosk:recognizer_new(M, 16000.0),
    vosk:recognizer_set_max_alternatives(R, 2),
    loop(R, Fd).

loop(R, Fd) ->
    case file:read(Fd, 16000) of  %% read one sec
	{ok,Bin} ->
	    case vosk:recognizer_accept_waveform(R, Bin) of
		0 ->
		    Result = vosk:recognizer_partial_result(R),
		    io:format("parital: ~p\n", [Result]),
		    loop(R, Fd);
		1 ->
		    Result = vosk:recognizer_result(R),
		    io:format("result: ~p\n", [Result]),
		    vosk:recognizer_reset(R),
		    loop(R, Fd)
	    end;
	eof ->
	    Result = vosk:recognizer_result_final(R),
	    io:format("parital: ~p\n", [Result]),
	    ok
    end.


	    
    

    
    
    
    
    
