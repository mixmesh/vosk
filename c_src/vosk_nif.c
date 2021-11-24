//
// Vosk NIF binding
//
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <errno.h>

#include "erl_nif.h"
#include "vosk_dl.h"
#define JSON_EXTERNAL_T ERL_NIF_TERM
#include "json.h"

#define MAX_PATH    1024
#define MAX_GRAMMAR 4096  // max json word array (fixme)
#define MAX_WORD    256

// #define DEBUG
// #define NIF_TRACE

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do {				\
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) {		\
	    emit_log((level),(file),(line),args);			\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#define BADARG(env) printf("matrix_nif.c: badarg line=%d\r\n", __LINE__), enif_make_badarg((env))
#else
#define DBG(...)
#define BADARG(env) enif_make_badarg((env))
#endif

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(env,name)			\
    atm_##name = enif_make_atom((env),#name)

#define LOAD_ATOM_STRING(env,name,string)	\
    atm_##name = enif_make_atom((env),string)

DECL_ATOM(ok);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(undefined);
DECL_ATOM(null);
DECL_ATOM(error);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
			 ERL_NIF_TERM load_info);
static void nif_unload(ErlNifEnv* env, void* priv_data);

#define NIF_LIST \
    NIF("model_new", 1, nif_model_new) \
    NIF("model_find_word",2,nif_model_find_word) \
    NIF("spk_model_new",1,nif_spk_model_new) \
    NIF("recognizer_new",2,nif_recognizer_new) \
    NIF("recognizer_new_spk",3,nif_recognizer_new_spk) \
    NIF("recognizer_new_grm",3,nif_recognizer_new_grm) \
    NIF("recognizer_set_max_alternatives",2,nif_recognizer_set_max_alternatives) \
    NIF("recognizer_set_words",2,nif_recognizer_set_words) \
    NIF("recognizer_accept_waveform",2,nif_recognizer_accept_waveform) \
    NIF("recognizer_result",1,nif_recognizer_result) \
    NIF("recognizer_partial_result",1,nif_recognizer_partial_result) \
    NIF("recognizer_final_result",1,nif_recognizer_final_result) \
    NIF("recognizer_reset",1,nif_recognizer_reset) \
    NIF("parse",1,nif_parse)

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

// may be needed to initailize gpu!?
#ifdef __APPLE__
extern int erl_drv_stolen_main_thread_join(ErlNifTid tid, void **respp);
extern int erl_drv_steal_main_thread(char *name,
				     ErlNifTid *dtid,
				     void* (*func)(void*),
				     void* arg,
				     ErlDrvThreadOpts *opts);
#endif


#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

typedef struct {
    enum {
	VOSK_MODEL,
	VOSK_SPK_MODEL,
	VOSK_RECOGNIZER
    } type;
    union {
	VoskModel* model;
	VoskSpkModel* spk_model;
	VoskRecognizer* recognizer;
    } ptr;
} vosk_object_t;

ErlNifResourceType* object_res;

typedef struct {
    int ref_count;
    int debug;      // current debug level
    int accel;      // current acceleration type
    json_t json;    // json parser data structure
} vosk_ctx_t;

static vosk_dl_t vosk_so;

static void load_atoms(ErlNifEnv* env,vosk_ctx_t* ctx);

int debug_level = DLOG_DEFAULT;

void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	int save_errno = errno;
	    
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
	errno = save_errno;
    }
}

void set_debug(int level)
{
    debug_level = level;
}

static void object_dtor(ErlNifEnv* env, void* obj)
{
    (void) env;
    switch(((vosk_object_t*)obj)->type) {
    case VOSK_MODEL:
	DEBUGF("VoskModel: destruct obj=%p", obj);
	vosk_so.vosk_model_free(((vosk_object_t*)obj)->ptr.model);
	break;
    case VOSK_SPK_MODEL:
	DEBUGF("VoskSpkModel: destruct obj=%p", obj);
	vosk_so.vosk_spk_model_free(((vosk_object_t*)obj)->ptr.spk_model);
	break;
    case VOSK_RECOGNIZER:
	DEBUGF("VoskRecognizer: destruct obj=%p", obj);
	vosk_so.vosk_recognizer_free(((vosk_object_t*)obj)->ptr.recognizer);
	break;
    }
}

static ERL_NIF_TERM nif_model_new(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    ERL_NIF_TERM r;
    VoskModel* model;
    vosk_object_t* obj;
    char model_path[MAX_PATH];

    if (!(r=enif_get_string(env, argv[0], model_path, sizeof(model_path),
			    ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);
    if ((model = vosk_so.vosk_model_new(model_path)) == NULL)
	return enif_make_badarg(env);
    obj = enif_alloc_resource(object_res, sizeof(vosk_object_t));
    obj->type = VOSK_MODEL;
    obj->ptr.model = model;
    r = enif_make_resource(env,obj);
    enif_release_resource(obj);
    return r;
}

static ERL_NIF_TERM nif_spk_model_new(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    ERL_NIF_TERM r;
    VoskSpkModel* spk_model;
    vosk_object_t* robj;
    char model_path[MAX_PATH];

    if (!(r=enif_get_string(env, argv[0], model_path, sizeof(model_path),
			    ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);
    if ((spk_model = vosk_so.vosk_spk_model_new(model_path)) == NULL)
	return enif_make_badarg(env);
    robj = enif_alloc_resource(object_res, sizeof(vosk_object_t));
    robj->type = VOSK_SPK_MODEL;
    robj->ptr.spk_model = spk_model;
    r = enif_make_resource(env,robj);
    enif_release_resource(robj);
    return r;
}

static ERL_NIF_TERM nif_recognizer_new(ErlNifEnv* env, int argc,
				      const ERL_NIF_TERM argv[])
{
    (void) argc;
    ERL_NIF_TERM r;
    VoskRecognizer* recognizer;
    vosk_object_t* obj;
    vosk_object_t* robj;
    double sample_rate;

    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_MODEL))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[1], &sample_rate)) {
	long rate;
	if (!enif_get_long(env, argv[1], &rate))
	    return enif_make_badarg(env);
	sample_rate = (double) rate;
    }
    if ((recognizer = vosk_so.vosk_recognizer_new(obj->ptr.model,
						  (float)sample_rate)) == NULL)
	return enif_make_badarg(env);

    robj = enif_alloc_resource(object_res, sizeof(vosk_object_t));
    robj->type = VOSK_RECOGNIZER;
    robj->ptr.recognizer = recognizer;
    r = enif_make_resource(env,robj);
    enif_release_resource(robj);
    return r;
}

static ERL_NIF_TERM nif_recognizer_new_spk(ErlNifEnv* env, int argc,
					   const ERL_NIF_TERM argv[])
{
    (void) argc;
    ERL_NIF_TERM r;
    VoskRecognizer* recognizer;
    vosk_object_t* obj;
    vosk_object_t* spk_obj;
    vosk_object_t* robj;
    double sample_rate;

    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_MODEL))
	return enif_make_badarg(env);    
    if (!enif_get_double(env, argv[1], &sample_rate)) {
	long rate;
	if (!enif_get_long(env, argv[1], &rate))
	    return enif_make_badarg(env);
	sample_rate = (double) rate;
    }    
    if (!enif_get_resource(env, argv[2], object_res, (void**) &spk_obj) ||
	(spk_obj->type != VOSK_SPK_MODEL))
	return enif_make_badarg(env);

    if ((recognizer = vosk_so.vosk_recognizer_new_spk(
	     obj->ptr.model,
	     (float)sample_rate,
	     spk_obj->ptr.spk_model)) == NULL)
	return enif_make_badarg(env);

    robj = enif_alloc_resource(object_res, sizeof(vosk_object_t));
    robj->type = VOSK_RECOGNIZER;    
    robj->ptr.recognizer = recognizer;
    r = enif_make_resource(env,robj);
    enif_release_resource(robj);
    return r;
}

static ERL_NIF_TERM nif_recognizer_new_grm(ErlNifEnv* env, int argc,
					   const ERL_NIF_TERM argv[])
{
    (void) argc;
    ERL_NIF_TERM r;
    VoskRecognizer* recognizer;
    vosk_object_t* obj;
    vosk_object_t* robj;
    double sample_rate;
    char grammar[MAX_GRAMMAR];

    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_MODEL))
	return enif_make_badarg(env);
    if (!enif_get_double(env, argv[1], &sample_rate)) {
	long rate;
	if (!enif_get_long(env, argv[1], &rate))
	    return enif_make_badarg(env);
	sample_rate = (double) rate;
    }
    if (!(r=enif_get_string(env, argv[2], grammar, sizeof(grammar),
			    ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);

    if ((recognizer = vosk_so.vosk_recognizer_new_grm(
	     obj->ptr.model,
	     (float)sample_rate,
	     grammar)) == NULL)
	return enif_make_badarg(env);

    robj = enif_alloc_resource(object_res, sizeof(vosk_object_t));
    robj->type = VOSK_RECOGNIZER;    
    robj->ptr.recognizer = recognizer;
    r = enif_make_resource(env,robj);
    enif_release_resource(robj);
    return r;
}

static ERL_NIF_TERM nif_model_find_word(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    char word[MAX_WORD];
    vosk_object_t* obj;
    int r;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_MODEL))
	return enif_make_badarg(env);
    if (!(r=enif_get_string(env, argv[1], word, sizeof(word),ERL_NIF_LATIN1))
	|| (r < 0))
	return enif_make_badarg(env);
    if (vosk_so.vosk_model_find_word(obj->ptr.model, word))
	return ATOM(true);
    return ATOM(false);
}

static ERL_NIF_TERM nif_recognizer_set_max_alternatives(ErlNifEnv* env, int argc,
					const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    int max_alternatives;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);	
    if (!enif_get_int(env, argv[1], &max_alternatives))
	return enif_make_badarg(env);
    vosk_so.vosk_recognizer_set_max_alternatives(obj->ptr.recognizer,
						 max_alternatives);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_recognizer_set_words(ErlNifEnv* env, int argc,
					     const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    int words;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);	
    if (!enif_get_int(env, argv[1], &words))
	return enif_make_badarg(env);
    vosk_so.vosk_recognizer_set_words(obj->ptr.recognizer, words);

    return ATOM(ok);
}

static ERL_NIF_TERM nif_recognizer_accept_waveform(ErlNifEnv* env, int argc,
						   const ERL_NIF_TERM argv[])
{
    (void) argc;    
    vosk_object_t* obj;
    ErlNifBinary bin;
    int r;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[1], &bin))
	return enif_make_badarg(env);	
    r = vosk_so.vosk_recognizer_accept_waveform(
	obj->ptr.recognizer, (const char*)bin.data, bin.size);
    return enif_make_int(env, r);
}

// callback that construct ERL_NIF_TERM from json data
static int json_callback(json_value_t* v, void* arg)
{
    ErlNifEnv* env = (ErlNifEnv*) arg;
    switch(v->type) {
    case JSON_NONE:
	v->ext = ATOM(undefined);
	break;
    case JSON_NULL:
	v->ext = ATOM(null);
	break;
    case JSON_TRUE:
	v->ext = ATOM(true);
	break;	
    case JSON_FALSE:
	v->ext = ATOM(false);
	break;
    case JSON_INTEGER:
	v->ext = enif_make_int(env, v->i);
	break;
    case JSON_FLOAT:
	v->ext = enif_make_double(env, v->f);
	break;	
    case JSON_STRING:
	v->ext = enif_make_string_len(env,v->s.ptr,v->s.len,ERL_NIF_LATIN1);
	break;
    case JSON_ARRAY: { // tuple? mimic jsone
	int i;
	ERL_NIF_TERM list = enif_make_list(env, 0);
	for (i = v->a.n-1; i >= 0; i--) { // build backwards
	    list = enif_make_list_cell(env, v->a.elem[i].ext, list);
	}
	v->ext = list;
	break;
    }
    case JSON_OBJECT: {
	int i;
	ERL_NIF_TERM map = enif_make_new_map(env);
	for (i = 0; i < v->o.n; i++) {
	    enif_make_map_put(env, map,
			      v->o.key[i].ext,
			      v->o.value[i].ext,&map);
	}
	v->ext = map;
	break;
    }
    default:
	break;
    }
    return 0;
}

static ERL_NIF_TERM json_to_term(ErlNifEnv* env, const char* jdata, size_t len)
{
    vosk_ctx_t* ctx = (vosk_ctx_t*) enif_priv_data(env);
    
    json_set_callback(&ctx->json, JSON_ALL_MASK, json_callback, env);
    if (json_parse(&ctx->json, (uint8_t*)jdata, len) < 0)
	return enif_make_badarg(env);
    return ctx->json.current.ext;
}

static ERL_NIF_TERM nif_recognizer_result(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    const char* jdata;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);
    jdata = vosk_so.vosk_recognizer_result(obj->ptr.recognizer);
    return json_to_term(env, jdata, strlen(jdata)+1);
}

static ERL_NIF_TERM nif_recognizer_partial_result(ErlNifEnv* env, int argc,
					  const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    const char* jdata;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);
    jdata = vosk_so.vosk_recognizer_partial_result(obj->ptr.recognizer);
    return json_to_term(env, jdata, strlen(jdata)+1);
}

static ERL_NIF_TERM nif_recognizer_final_result(ErlNifEnv* env, int argc,
						const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    const char* jdata;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);
    jdata = vosk_so.vosk_recognizer_final_result(obj->ptr.recognizer);
    return json_to_term(env, jdata, strlen(jdata)+1);
}

static ERL_NIF_TERM nif_recognizer_reset(ErlNifEnv* env, int argc,
					 const ERL_NIF_TERM argv[])
{
    (void) argc;
    vosk_object_t* obj;
    
    if (!enif_get_resource(env, argv[0], object_res, (void**) &obj) ||
	(obj->type != VOSK_RECOGNIZER))
	return enif_make_badarg(env);
    vosk_so.vosk_recognizer_reset(obj->ptr.recognizer);
    return ATOM(ok);
}

// Test function - json parser
static ERL_NIF_TERM nif_parse(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    (void) argc;
    char data[4096];
    int n;

    n = enif_get_string(env, argv[0], data, sizeof(data),ERL_NIF_LATIN1);
    if (n <= 0)
	return enif_make_badarg(env);
    return json_to_term(env, data, n);
}

// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static void load_atoms(ErlNifEnv* env,vosk_ctx_t* ctx)
{
    (void) ctx;
    // Load atoms
    LOAD_ATOM(env,ok);
    LOAD_ATOM(env,true);
    LOAD_ATOM(env,false);
    LOAD_ATOM(env,undefined);
    LOAD_ATOM(env,null);
    LOAD_ATOM(env,error);
}

static void dl_error(void* arg,const char* what)
{
    fprintf(stderr, "vosk dl error: %s\r\n", what);
}
    
static int load_vosk_so(const char* lib, vosk_dl_t* dlp)
{
    void* handle;

    if ((handle = enif_dlopen(lib, dl_error, NULL)) == NULL)
	return -1;
    dlp->vosk_model_new = enif_dlsym(handle, "vosk_model_new", dl_error, NULL);
    dlp->vosk_model_free = enif_dlsym(handle, "vosk_model_free", dl_error, NULL);
    dlp->vosk_model_find_word = enif_dlsym(handle, "vosk_model_find_word", dl_error, NULL);
    dlp->vosk_spk_model_new = enif_dlsym(handle, "vosk_spk_model_new", dl_error, NULL);
    dlp->vosk_spk_model_free = enif_dlsym(handle, "vosk_spk_model_free", dl_error, NULL);
    dlp->vosk_recognizer_new = enif_dlsym(handle, "vosk_recognizer_new", dl_error, NULL);
    dlp->vosk_recognizer_new_spk = enif_dlsym(handle, "vosk_recognizer_new_spk", dl_error, NULL);
    dlp->vosk_recognizer_new_grm = enif_dlsym(handle, "vosk_recognizer_new_grm", dl_error, NULL);
    dlp->vosk_recognizer_set_spk_model = enif_dlsym(handle, "vosk_recognizer_set_spk_model", dl_error, NULL);
    dlp->vosk_recognizer_set_max_alternatives = enif_dlsym(handle, "vosk_recognizer_set_max_alternatives", dl_error, NULL);
    dlp->vosk_recognizer_set_words = enif_dlsym(handle, "vosk_recognizer_set_words", dl_error, NULL);
    dlp->vosk_recognizer_accept_waveform = enif_dlsym(handle, "vosk_recognizer_accept_waveform", dl_error, NULL);
    dlp->vosk_recognizer_accept_waveform_s = enif_dlsym(handle, "vosk_recognizer_accept_waveform_s", dl_error, NULL);
    dlp->vosk_recognizer_accept_waveform_f = enif_dlsym(handle, "vosk_recognizer_accept_waveform_f", dl_error, NULL);
    dlp->vosk_recognizer_result = enif_dlsym(handle, "vosk_recognizer_result", dl_error, NULL);
    dlp->vosk_recognizer_partial_result = enif_dlsym(handle, "vosk_recognizer_partial_result", dl_error, NULL);
    dlp->vosk_recognizer_final_result = enif_dlsym(handle, "vosk_recognizer_final_result", dl_error, NULL);
    dlp->vosk_recognizer_reset = enif_dlsym(handle, "vosk_recognizer_reset", dl_error, NULL);
    dlp->vosk_recognizer_free = enif_dlsym(handle, "vosk_recognizer_free", dl_error, NULL);
    dlp->vosk_set_log_level = enif_dlsym(handle, "vosk_set_log_level", dl_error, NULL);
    dlp->vosk_gpu_init = enif_dlsym(handle, "vosk_gpu_init", dl_error, NULL);
    dlp->vosk_gpu_thread_init = enif_dlsym(handle, "vosk_gpu_thread_init", dl_error, NULL);
    return 0;
}

static json_key_t keytab[] = 
{
    { "word" },
    { "start" },
    { "end" },
    { "conf" },
    { "result" },
    { "text" },
    { "confidence" },
    { "alternatives" },
    { "spk" },
    { "spk_frames" },
};


static int nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags tried;
    vosk_ctx_t* ctx;
    (void) env;
    int r;
    char libpath[MAX_PATH];

    if (!(r = enif_get_string(env, load_info, libpath, sizeof(libpath),
			      ERL_NIF_LATIN1)) || (r < 0))
	return -1;

    //set_debug(DLOG_DEFAULT);

    DEBUGF("nif_load: libpath=%s", libpath);

    // Create resource types
    object_res = enif_open_resource_type(env, 0, "vosk_object",
					 object_dtor,
					 ERL_NIF_RT_CREATE, &tried);

    if ((ctx = (vosk_ctx_t*) enif_alloc(sizeof(vosk_ctx_t))) == NULL)
	return -1;
    ctx->ref_count = 1;
    ctx->debug = DLOG_DEFAULT;
    ctx->accel = 0; // gpu init? status?

    load_atoms(env, ctx);
    load_vosk_so(libpath, &vosk_so);

    json_init(&ctx->json, JSON_ALL_MASK, json_callback, env);
    json_set_keys(&ctx->json, keytab, sizeof(keytab)/sizeof(keytab[0]), 1000);

    *priv_data = ctx;
    return 0;
}

static int nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		       ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    ErlNifResourceFlags tried;
    vosk_ctx_t* ctx = (vosk_ctx_t*) *old_priv_data;
    int r;
    char libpath[MAX_PATH];

    if (!(r=enif_get_string(env, load_info, libpath, sizeof(libpath),
			    ERL_NIF_LATIN1)) || (r < 0))
	return -1;

    // set_debug(ctx->debug);
    DEBUGF("nif_upgrade: libpath=%s", libpath);
    // vosk_init(ctx->accel);
    ctx->ref_count++;
    object_res = enif_open_resource_type(env, 0, "vosk_object",
					 object_dtor,
					 ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
					 &tried);
    load_atoms(env, ctx);
    load_vosk_so(libpath, &vosk_so);
    
    *priv_data = *old_priv_data;
    return 0;
}

static void nif_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    vosk_ctx_t* ctx = (vosk_ctx_t*) priv_data;

    DEBUGF("nif_unload");
    enif_free(ctx);
}


ERL_NIF_INIT(vosk, nif_funcs,
	     nif_load, NULL,
	     nif_upgrade, nif_unload)

