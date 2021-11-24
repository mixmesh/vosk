%% Test vosk
%%
-module(vosk_test).
-export([start/0, start/2]).
-export([check_word/1, check_word/2]).
-export([check_words/1, check_words/2]).

start() ->
    start("test.wav", en).

start(WavFilename, Lang) ->
    {ok,Fd} = file:open(WavFilename, [raw,read,binary]),
    _ = file:read(Fd, 48),  %% skip header
    M = vosk:model_new(filename:join(code:priv_dir(vosk),select_model(Lang))),
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

check_word(Word) ->
    check_words([Word]).
check_word(Word,Lang) ->
    check_words([Word],Lang).

%% check if word is labled in model
check_words(Words) ->
    check_words(Words,en).
check_words(Words, Lang) ->
    M = vosk:model_new(filename:join(code:priv_dir(vosk),select_model(Lang))),
    [{Word,vosk:model_find_word(M, Word)} || Word <- Words].

select_model(sv) ->	    
    "vosk-model-small-sv-rhasspy-0.15";
select_model(en) ->	
    "vosk-model-small-en-us-0.15".


    

    
    
    
    
    
