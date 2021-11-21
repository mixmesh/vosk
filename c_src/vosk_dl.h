// Vosk shared object wrapper

#ifndef __VOSK_DL__
#define __VOSK_DL__ 1

typedef struct VoskModel VoskModel;
typedef struct VoskSpkModel VoskSpkModel;
typedef struct VoskRecognizer VoskRecognizer;

typedef struct vosk_dl_ {
    VoskModel* (*vosk_model_new)(const char *model_path);
    void (*vosk_model_free)(VoskModel *model);
    int (*vosk_model_find_word)(VoskModel *model, const char *word);
    VoskSpkModel* (*vosk_spk_model_new)(const char *model_path);
    void (*vosk_spk_model_free)(VoskSpkModel *model);
    VoskRecognizer* (*vosk_recognizer_new)(VoskModel *model, float sample_rate);
    VoskRecognizer* (*vosk_recognizer_new_spk)(VoskModel *model, float sample_rate, VoskSpkModel *spk_model);
    VoskRecognizer* (*vosk_recognizer_new_grm)(VoskModel *model, float sample_rate, const char *grammar);
    void (*vosk_recognizer_set_spk_model)(VoskRecognizer *recognizer, VoskSpkModel *spk_model);
    void (*vosk_recognizer_set_max_alternatives)(VoskRecognizer *recognizer, int max_alternatives);
    void (*vosk_recognizer_set_words)(VoskRecognizer *recognizer, int words);
    int (*vosk_recognizer_accept_waveform)(VoskRecognizer *recognizer, const char *data, int length);
    int (*vosk_recognizer_accept_waveform_s)(VoskRecognizer *recognizer, const short *data, int length);
    int (*vosk_recognizer_accept_waveform_f)(VoskRecognizer *recognizer, const float *data, int length);
    const char* (*vosk_recognizer_result)(VoskRecognizer *recognizer);
    const char* (*vosk_recognizer_partial_result)(VoskRecognizer *recognizer);
    const char* (*vosk_recognizer_final_result)(VoskRecognizer *recognizer);
    void (*vosk_recognizer_reset)(VoskRecognizer *recognizer);
    void (*vosk_recognizer_free)(VoskRecognizer *recognizer);
    void (*vosk_set_log_level)(int log_level);
    void (*vosk_gpu_init)();  // main thread once
    void (*vosk_gpu_thread_init)(); // every thread
} vosk_dl_t;

#endif
