%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created : 21 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(vosk).

-on_load(init/0).
-export([model_new/1]).
-export([model_find_word/2]).
-export([spk_model_new/1]).
-export([recognizer_new/2]).
-export([recognizer_new_spk/3]).
-export([recognizer_new_grm/3]).
-export([recognizer_set_max_alternatives/2]).
-export([recognizer_set_words/2]).
-export([recognizer_accept_waveform/2]).
-export([recognizer_result/1]).
-export([recognizer_partial_result/1]).
-export([recognizer_final_result/1]).
-export([recognizer_reset/1]).
-export([parse/1]).

-export_type([vosk_model/0]).
-export_type([vosk_spk_model/0]).
-export_type([vosk_recognizer/0]).

-type vosk_model()      :: reference().   %% nif resource
-type vosk_spk_model()  :: reference().   %% nif resource
-type vosk_recognizer() :: reference().   %% nif resource
-type void() :: ok.

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).


init() ->
    Nif = filename:join(code:priv_dir(vosk), "vosk_nif"),
    Dll = filename:join(code:priv_dir(vosk), "libvosk"),
    %% pass full path of libvosk.so
    erlang:load_nif(Nif, Dll).

-spec model_new(ModelPath::string()) -> vosk_model().
model_new(_ModelPath) ->
    ?nif_stub().

-spec model_find_word(Model::vosk_model(), Word::string()) -> boolean().

model_find_word(_Model, _Word) ->
    ?nif_stub().    

-spec spk_model_new(ModelPath::string()) -> vosk_spk_model().
spk_model_new(_ModelPath) ->
    ?nif_stub().

-spec recognizer_new(Model::vosk_model(), SampleRate::float()) ->
	  vosk_recognizer().

recognizer_new(_VoskModel, _SampleRate) ->
    ?nif_stub().

-spec recognizer_new_spk(Model::vosk_model(), SampleRate::float(),
			 SpkModel::vosk_spk_model()) ->
	  vosk_recognizer().
    
recognizer_new_spk(_Model, _SampleRate, _SpkModel) ->
    ?nif_stub().

-spec recognizer_new_grm(Model::vosk_model(), SampleRate::float(),
			 Grammar::string()) ->
	  vosk_recognizer().
    
recognizer_new_grm(_Model, _SampleRate, _Grammar) ->
    ?nif_stub().


-spec recognizer_set_max_alternatives(Recognizer::vosk_recognizer(),
					   MaxAlternatives::integer()) ->
	  void().

recognizer_set_max_alternatives(_Recognizer, _MaxAlternatives) ->
    ?nif_stub().

-spec recognizer_set_words(Recognizer::vosk_recognizer(), 
				Words::boolean()) ->
	  void().

recognizer_set_words(_Recognizer, _Words) ->
    ?nif_stub().

%% audio data in LITTLE-ENDIAN PCM 16-bit mono format
%% return 
%%     1 if silence is occured and you can retrieve a new utterance 
%%       with result method 
%%     0 if decoding continues
%%    -1 if exception occured */

-spec recognizer_accept_waveform(Recognizer::vosk_recognizer(),
				 Data::binary()) ->
	  integer().

recognizer_accept_waveform(_Recognizer, _Data) ->
    ?nif_stub().


-spec recognizer_result(Recognizer::vosk_recognizer()) ->
	  map().

recognizer_result(_Recognizer) ->
    ?nif_stub().

-spec recognizer_partial_result(Recognizer::vosk_recognizer()) ->
	  map().

recognizer_partial_result(_Recognizer) ->
    ?nif_stub().

-spec recognizer_final_result(Recognizer::vosk_recognizer()) ->
	  map().

recognizer_final_result(_Recognizer) ->
    ?nif_stub().
    
-spec recognizer_reset(Recognizer::vosk_recognizer()) -> void().

recognizer_reset(_Recognizer) ->
    ?nif_stub().

%%
%% Test stuff
%%
-spec parse(String::string()) -> term().

parse(_String) ->
    ?nif_stub().
