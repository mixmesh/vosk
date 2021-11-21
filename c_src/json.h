//
// (relaxed) reentrant JSON parser API
//
// the idea is to parse and callback for every object
// the return value from the callback determine if the object 
// is discarded from heap or not (allow for top level objects)
//
#ifndef __JSON_H__
#define __JSON_H__

#include <stdint.h>

struct _json_value_t;

typedef enum {
    JSON_NONE,
    // object types
    JSON_NULL,
    JSON_FALSE,
    JSON_TRUE,
    JSON_INTEGER,
    JSON_FLOAT,
    JSON_STRING,
    JSON_ARRAY,
    JSON_OBJECT,
    // extra marks
    JSON_EXTERNAL,  // external object
    // parse marks
    JSON_PAIR_MARK,
    JSON_ARRAY_MARK,
    JSON_OBJECT_MARK,
} json_type_t;

#define JSON_ALL_MASK \
    ((1 << JSON_NULL) | (1 << JSON_FALSE) | (1 << JSON_TRUE) |		\
     (1<< JSON_INTEGER) | (1 << JSON_FLOAT) | (1 << JSON_STRING) |	\
     (1 << JSON_ARRAY) | (1 << JSON_OBJECT))

typedef struct {
    char*    ptr;  // pointer to chars
    unsigned len;  // length of string (not including '\0')
    unsigned key;  // key number 1..n, 0 => not a key
} json_string_t;

#ifndef JSON_EXTERNAL_T
#define JSON_EXTERNAL_T uintptr_t
#endif

typedef struct _json_value_t {
    json_type_t type;
    union {
	long   i;
	double f;
	json_string_t s;
	struct {  // [ v1, v2, .., vn] 
	    unsigned n;
	    struct _json_value_t* elem;
	} a;
	struct {  // { k1:v1, k2:v2, ... kn:vn }
	    unsigned n;
	    struct _json_value_t* key;   // restricted to JSON_STRING!
	    struct _json_value_t* value;
	} o;
	struct {
	    int hp;
	    json_type_t ctx;
	} x;
	JSON_EXTERNAL_T ext;
    };
} json_value_t;

// keyword entries
typedef struct
{
    char*    ptr;   // pointer to chars, should terminated with 0
    unsigned key;   // key number 1..n, 0 => not a key
    unsigned len;   // length of string not including 0
    unsigned hash;  // key hash value
    unsigned index; // hash slot index
} json_key_t;

#ifndef JSON_MAX_HEAP_SIZE
#define JSON_MAX_HEAP_SIZE (64*1024)
#endif

#ifndef JSON_MAX_PARSE_DEPTH
#define JSON_MAX_PARSE_DEPTH  1024
#endif

// reentrant state of tokenizer
typedef enum {
    JSON_ST_NONE,
    JSON_ST_STRING,      // in "   string
    JSON_ST_BQUOTE,      // \ "    string
    JSON_ST_U1,          // \u     string
    JSON_ST_U2,          // \uA    string
    JSON_ST_U3,          // \uAB   string
    JSON_ST_U4,          // \uABC  string
    JSON_ST_IDENTIFIER,  // in identifier (relaxed)
    JSON_ST_INTEGER,     // in integer
    JSON_ST_MINUS,       // '-' seen
    JSON_ST_FRAC,        // after '.'
    JSON_ST_E,           // after e|E
    JSON_ST_ESIGN,       // affter (e|E)(+|-)
    JSON_ST_EXP,         // in exponent
    JSON_ST_SLASH,       // '/' seen
    JSON_ST_LCOMMENT,    // '//' comment
    JSON_ST_BCOMMENT,    // '/*' comment
    JSON_ST_BSTAR,       // '*' seen in block comment
} json_state_t;

typedef int (*json_callback_t)(json_value_t* v, void* arg);

typedef struct _json_t
{
    json_state_t state;
    json_type_t  ctx;
    unsigned cb_mask;     // callback type bitmask
    json_callback_t cb;
    void* cb_arg;
    uint8_t heap[JSON_MAX_HEAP_SIZE];
    json_value_t stack[JSON_MAX_PARSE_DEPTH];
    json_value_t current;
    size_t nkeys;          // number of keys in key-table
    json_key_t* keys;      // defined keys
    unsigned h_true;
    unsigned h_false;
    unsigned h_null;
    int relaxed;
    int line;
    unsigned hash;         // current string hash value
    int sp;
    int hp;
} json_t;

extern void json_init(json_t* jp, unsigned cb_mask,
		      json_callback_t cb, void* cb_arg);
extern int json_set_keys(json_t* jp, json_key_t* keys, size_t size,
			 unsigned auto_offset);
extern void json_set_callback(json_t* jp, unsigned cb_mask, 
			      json_callback_t cb, void* cb_arg);
extern void json_set_relaxed(json_t* jp, int relaxed);
extern int json_parse(json_t* jp, uint8_t* ptr, size_t len);
extern void json_print(FILE* f, json_value_t* vp);
extern void json_print_string(FILE* f, char* ptr, unsigned len);
extern void json_print_raw(FILE* f, char* ptr, unsigned len);

#endif
