# Download, unzip and copy libraries
URL=https://alphacephei.com/kaldi/models
SV_MODEL=vosk-model-small-sv-rhasspy-0.15
EN_MODEL=vosk-model-small-en-us-0.15
PYTHON3=python3.9
LIBATOMIC=libatomic-1abecf58.so.1.2.0
LIBVOSK=libvosk.so

download:
	wget $(URL)/$(MODELS_URL)/$(SV_MODEL).zip
	wget $(URL)/$(MODELS_URL)/$(EN_MODEL).zip
	unzip $(SV_MODEL).zip
	unzip $(EN_MODEL).zip

py_install:
	 pip3 install sounddevice
	 pip3 install vosk

copy_so:
#	cp $(HOME)/.local/lib/$(PYTHON3)/site-packages/vosk.libs/$(LIBATOMIC) .
	cp $(HOME)/.local/lib/$(PYTHON3)/site-packages/vosk/$(LIBVOSK) .
