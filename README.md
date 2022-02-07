# vosk

# installation 

Currently the python implmentation is used to get the 
binary shared object and avoid the hassle of compilation.

The steps are captured in a Makefile

    cd priv
	make py_install
	make copy_so
	make download
	
The py\_install will pip3 install sounddevice and vosk.
copy\_so copies the shared objects from the local lib
to priv directory and 'download' will download langauge
models for swedish and english.

# example use

	IMPORTANT IMPORTANT IMPORTANT IMPORTANT
	LD_LIBRARY_PATH must be set!

	$ export LD_LIBRARY_PATH=$HOME/erlang/vosk/priv
	$ erl
	> Model = vosk:model_new(filename:join(code:priv_dir(vosk), "vosk-model-small-en-us-0.15")).
	> Recognizer = vosk:recognizer_new(Model, 16000).
	> {ok,<<_:48/binary,Samples:64000/binary,_/binary>>} = file:read_file("test.wav").
	> vosk:recognizer_partial_result(Recognizer, Samples).
