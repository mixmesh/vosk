//
// (relaxed) reentrant JSON parser
//

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "json.h"

#define align(h) (((h)+sizeof(void*)) & ~(sizeof(void*)-1))

// print as hex string (debugging)
void json_print_raw(FILE* f, char* ptr, unsigned len)
{
    int i;
    fputc('"', f);
    for (i = 0; i < len; i++) {
	int c = (uint8_t) ptr[i];
	fprintf(f, "\\u%04x", c);
    }
    fputc('"', f);
}

void json_print_string(FILE* f, char* ptr, unsigned len)
{
    int i;
    fputc('"', f);
    for (i = 0; i < len; i++) {
	int c = (uint8_t) ptr[i];
	switch(c) {
	case '"': fputc('\\', f); fputc('"', f); break;
	case '\\': fputc('\\', f); fputc('\\', f); break;
	case '/': fputc('\\', f); fputc('/', f); break;
	case '\b': fputc('\\', f); fputc('b', f); break;
	case '\f': fputc('\\', f); fputc('f', f); break;
	case '\n': fputc('\\', f); fputc('n', f); break;
	case '\r': fputc('\\', f); fputc('r', f); break;
	case '\t': fputc('\\', f); fputc('t', f); break;
	default:
	    if (isprint(c))
		fputc(c, f);
	    else 
		fprintf(f, "\\u%04x", c);
	    break;
	}
    }
    fputc('"', f);
}

void json_print(FILE* f, json_value_t* vp)
{
    int i;
    switch(vp->type) {
    case JSON_NONE: fprintf(f,"?"); break;
    case JSON_NULL: fprintf(f,"null"); break;
    case JSON_TRUE: fprintf(f,"true"); break;
    case JSON_FALSE: fprintf(f,"false"); break;
    case JSON_INTEGER: fprintf(f,"%ld", vp->i); break;
    case JSON_FLOAT: fprintf(f,"%f", vp->f); break;
    case JSON_STRING: json_print_string(f, vp->s.ptr, vp->s.len); break;
    case JSON_ARRAY:
	fprintf(f,"[");
	for (i = 0; i < vp->a.n; i++) {
	    if (i) fprintf(f,",");
	    json_print(f,&vp->a.elem[i]);
	}
	fprintf(f,"]");
	break;
    case JSON_OBJECT:
	fprintf(f,"{");
	for (i = 0; i < vp->o.n; i++) {
	    if (i) fprintf(f,",");
	    fprintf(f,"\"%s\":", vp->o.key[i].s.ptr);
	    json_print(f,&vp->o.value[i]);
	}
	fprintf(f,"}");
	break;
    case JSON_EXTERNAL:
	fprintf(f,"%p", (void*) vp->ext);
	break;
    case JSON_PAIR_MARK:
	fprintf(f,"(:)");
	break;
    case JSON_ARRAY_MARK:
	fprintf(f,"([])");
	break;
    case JSON_OBJECT_MARK:
	fprintf(f,"({})");
	break;
    }
}

#ifdef DEBUG
static void print_tos(json_value_t* stack, int sp, int hp)
{
    printf("%d> ",hp);
    if (sp==0)
	printf("empty\n");
    else {
	int i;
	for (i = sp-1; i >= 0; i--) {
	    json_print(stdout, &stack[i]);
	    printf(" ");
	}
	printf("\n");
    }
}
#endif


#define djb_hash_init(h) do { h = 5381; } while(0)
#define djb_hash_step(h,c) do { \
	h = ((h << 5) + h) + (c);		\
    } while(0)

unsigned djb_hash(char* str, unsigned len)
{
    unsigned hash;
    djb_hash_init(hash);
    while(len--) {
	djb_hash_step(hash,*str++);
    }
    return hash;
}

unsigned json_find_key(json_t* jp, unsigned hash,
		       char* ptr, unsigned len)
{
    unsigned i;
    // bloom filter ?
    for (i=0; i < jp->nkeys; i++) {
	if ((jp->keys[i].hash == hash) &&
	    (jp->keys[i].len == len) &&
	    (memcmp(jp->keys[i].ptr, ptr, len) == 0))
	    return jp->keys[i].key;
    }
    return 0;
}
//
// Initialize reentrant (relaxed) json parser
// 
void json_init(json_t* jp, unsigned cb_mask, json_callback_t cb, void* cb_arg)
{
    jp->state = JSON_ST_NONE;
    jp->ctx = JSON_NONE;
    jp->current.type = JSON_NONE;
    jp->relaxed = 1;
    jp->line = 1;
    jp->sp = 0;
    jp->hp = 0;
    jp->nkeys = 0;
    jp->keys = NULL;
    jp->cb_mask = cb_mask;
    jp->cb = cb;
    jp->cb_arg = cb_arg;
    jp->h_true = djb_hash("true", 4);
    jp->h_false = djb_hash("false", 5);
    jp->h_null = djb_hash("null", 4);
}

void json_set_callback(json_t* jp, unsigned cb_mask, 
		       json_callback_t cb, void* cb_arg)
{
    jp->cb_mask = cb_mask;
    jp->cb = cb;
    jp->cb_arg = cb_arg;
}

void json_set_relaxed(json_t* jp, int relaxed)
{
    jp->relaxed = relaxed;
}

int json_set_keys(json_t* jp, json_key_t* keys, size_t size,
		  unsigned auto_offset)
{
    unsigned i;

    for (i = 0; i < size; i++) {
	if (keys[i].ptr != NULL) {
	    if (keys[i].len == 0)
		keys[i].len = strlen((char*)keys[i].ptr);
	    keys[i].hash = djb_hash(keys[i].ptr, keys[i].len);
	}
	if (keys[i].key == 0) // auto enumerate
	    keys[i].key = auto_offset+i;
#ifdef DEBUG
	if (keys[i].ptr != NULL)
	    printf("set key[%d] ptr=%s, len=%u, key=%u, hash=%u\n",
		   i, keys[i].ptr, keys[i].len, keys[i].key, keys[i].hash);
	else
	    printf("set key[%d] ptr=NULL, len=0, key=%u, hash=%u\n",
		   i, keys[i].key, keys[i].hash);
#endif
    }
    jp->nkeys = size;
    jp->keys = keys;
    return 0;
}

#define SWAPIN(p) do {				\
	stack = (p)->stack;			\
	heap = (p)->heap;			\
	ctx = (p)->ctx;				\
	state = (p)->state;			\
	hp = (p)->hp;				\
	sp = (p)->sp;				\
	hash = (p)->hash;			\
    } while(0)

#define SWAPOUT(p) do {				\
	(p)->state = state;			\
	(p)->ctx = ctx;				\
	(p)->hp = hp;				\
	(p)->sp = sp;				\
	(p)->hash = hash;			\
    } while(0)

#define GOTO(label) do { state = label; goto label; } while(0)

#define CURRENT_STRING(jp) do {				\
	(jp)->current.type = JSON_STRING;		\
	(jp)->current.s.len = 0;			\
	(jp)->current.s.ptr = (char*) &heap[hp];	\
	djb_hash_init(hash);				\
    } while(0)

#define SET_CURRENT_STRING(jp,str) do {			\
	CURRENT_STRING(jp);				\
	strcpy((char*)&heap[hp],(str));			\
	hp += strlen((str));				\
    } while(0)

#define CURRENT_INTEGER(jp) do {			\
	(jp)->current.type = JSON_INTEGER;		\
	(jp)->current.i = hp;				\
    } while(0)

int hex_digit(int c) 
{
    switch(c) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	return c-'0';
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	return (c-'A')+10;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	return (c-'a')+10;
    default:
	return -1;
    }
}

// maybe callback on current value, reset heap and null if
// callback return 1
#define CALLBACK(hp0) do {						\
	if (jp->cb_mask & (1 << jp->current.type)) {			\
	    if ((*jp->cb)(&jp->current,jp->cb_arg)) {			\
		jp->current.type = JSON_NULL;				\
		hp = (hp0);						\
	    }								\
	}								\
	if ((sp==0)||(ctx==JSON_NONE)) jp->current.type = JSON_NONE;	\
	state = JSON_ST_NONE;						\
    } while(0)

#define CHECK_SP(sp) do { \
	if ((sp) >= JSON_MAX_PARSE_DEPTH) goto stack_overflow;	\
    } while(0)

#define CHECK_HP(hp) do { \
	if ((hp) >= JSON_MAX_HEAP_SIZE) goto heap_overflow;	\
    } while(0)


int json_parse(json_t* jp, uint8_t* ptr, size_t len)
{
    // context data
    json_value_t* stack;
    uint8_t* heap;
    json_type_t ctx;
    json_state_t state;
    int hp;
    int sp;
    unsigned hash;
    // local data
    uint8_t* ptr_end;

    ptr_end = ptr + len;
    SWAPIN(jp);

    goto more;

next:
    CHECK_SP(sp);
    CHECK_HP(hp);
#ifdef DEBUG
    print_tos(stack, sp, hp);
#endif
more:
    while(ptr < ptr_end) {
	switch(state) {
	case JSON_ST_NONE:
	    switch(*ptr) {
	    case ' ': case '\t': case '\r':  // ignore blank
		ptr++;
		continue;
	    case '\n': 
		ptr++;
		jp->line++;  // but count line number
		continue;
	    case '\0':       // end-marker (fixme space as end-marker?)
		ptr++;
		continue;
	    case '/':  // comments 
		if (jp->relaxed) {
		    ptr++;
		    GOTO(JSON_ST_SLASH);
		}
		goto lex_error;
	    case '[':
		if (jp->current.type != JSON_NONE) goto syntax_error;
		ptr++;
		stack[sp].x.hp = hp;   // save heap pointer for reset
		stack[sp].x.ctx = ctx; // save current context
		stack[sp].type = JSON_ARRAY_MARK;
		sp++;
		ctx = JSON_ARRAY;
		goto next;
	    case ']': {
		int i;
		if (ctx != JSON_ARRAY)
		    goto syntax_error;
		ptr++;
		if (jp->current.type != JSON_NONE)
		    stack[sp++] = jp->current;
		i = sp-1;
		// stack: '[' v1 v2 .. vn
		while(i>=0) {
		    if (stack[i].type == JSON_ARRAY_MARK) {
			int hp0;
			unsigned len = (sp-1)-(i+1)+1;
			json_value_t* elem;
			size_t sz = sizeof(json_value_t)*len;
			hp = align(hp);
			CHECK_HP(hp+sz);
			elem = (json_value_t*) &heap[hp];
			memcpy(elem, &stack[i+1], sz);
			hp += sz;
			hp0 = stack[i].x.hp;
			ctx = stack[i].x.ctx;
			// hp = stack[i].x.hp;  must check callback!
			jp->current.type = JSON_ARRAY;
			jp->current.a.n  = len;
			jp->current.a.elem = elem;
			sp = i;
			CALLBACK(hp0);
			goto next;
		    }
		    else if ((stack[i].type == JSON_PAIR_MARK) ||
			     (stack[i].type == JSON_OBJECT_MARK))
			goto syntax_error;
		    i--;
		}
		break;
	    }

	    case '{':
		if (jp->current.type != JSON_NONE) goto syntax_error;
		ptr++;
		stack[sp].x.hp = hp;   // save heap pointer for reset
		stack[sp].x.ctx = ctx; // save current context
		stack[sp].type = JSON_OBJECT_MARK;
		sp++;
		ctx = JSON_OBJECT;
		goto next;

	    case '}': {
		int i;
		if (ctx != JSON_OBJECT) goto syntax_error;
		if (jp->current.type != JSON_NONE)
		    stack[sp++] = jp->current;
		ptr++;
		i = sp-1;
		// stack: '{' k1 ':' v1 k2 ':' v2 ...  kn ':' vn
		// rebuild into: k1 k2 .. kn v1 v2 .. vn
		while(i>=0) {
		    if (stack[i].type == JSON_OBJECT_MARK) {
			int hp0;
			int j;
			unsigned n = ((sp-1)-(i+1)+1) / 3;
			size_t sz = 2*sizeof(json_value_t)*n;
			json_value_t* key;
			json_value_t* value;
			hp0 = stack[i].x.hp;
			ctx = stack[i].x.ctx;  // restore context
			hp = align(hp);
			CHECK_HP(hp+sz);
			key = (json_value_t*) &heap[hp];
			value = key + n;
			for (j = 0; j < n; j++) {
			    key[j] = stack[i+3*j+1];
			    value[j] = stack[i+3*j+3];
			}
			hp += sz;
			jp->current.type = JSON_OBJECT;
			jp->current.o.n  = n;
			jp->current.o.key = key;
			jp->current.o.value= value;
			sp = i;
			CALLBACK(hp0);
			goto next;
		    }
		    else if ((stack[i].type == JSON_PAIR_MARK) ||
			     (stack[i].type == JSON_ARRAY_MARK))
			goto syntax_error;
		    else { // key ':' value 
			if ((stack[i-1].type == JSON_PAIR_MARK) &&
			    (stack[i-2].type == JSON_STRING)) {
			}
			else 
			    goto syntax_error;
			i -= 3;
		    }
		}
		break;
	    }

	    case '-':
		if (jp->current.type != JSON_NONE) goto syntax_error;
		CURRENT_INTEGER(jp);
		jp->heap[hp++] = *ptr++;
		GOTO(JSON_ST_MINUS);

	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
		if (jp->current.type != JSON_NONE) goto syntax_error;
		CURRENT_INTEGER(jp);
		GOTO(JSON_ST_INTEGER);

	    case '"':
		if (jp->current.type != JSON_NONE) goto syntax_error;
		ptr++;
		CURRENT_STRING(jp);
		GOTO(JSON_ST_STRING);

	    case ':':
		if (ctx != JSON_OBJECT) goto syntax_error;
		if (jp->current.type != JSON_STRING) goto syntax_error;
		ptr++;
		stack[sp] = jp->current; // push key onto the stack
		sp++;
		CHECK_SP(sp);
		stack[sp].type = JSON_PAIR_MARK; // mark with pair-mark
		sp++;
		jp->current.type = JSON_NONE;      // prepare for value
		goto next;

	    case ',':
		if ((ctx != JSON_OBJECT) && (ctx != JSON_ARRAY))
		    goto syntax_error;
		if (jp->current.type == JSON_NONE) goto syntax_error;
		stack[sp] = jp->current;     // push value onto stack
		sp++;
		jp->current.type = JSON_NONE;  // prepare for next value
		ptr++;
		goto next;

	    default:
		if (isalpha(*ptr)) {
		    CURRENT_STRING(jp);
		    GOTO(JSON_ST_IDENTIFIER);
		}
		goto lex_error;
	    }
	    break;

	case JSON_ST_SLASH:
	JSON_ST_SLASH:
	    if (ptr < ptr_end) {
		switch(*ptr) {
		case '/':
		    ptr++;
		    GOTO(JSON_ST_LCOMMENT);
		case '*':
		    ptr++;
		    GOTO(JSON_ST_BCOMMENT);
		default:
		    goto lex_error;
		}
	    }
	    goto swapout;

	case JSON_ST_LCOMMENT:
	JSON_ST_LCOMMENT:
	    while((ptr<ptr_end) && (*ptr != '\n'))
		ptr++;
	    if (ptr == ptr_end) goto swapout;
	    state = JSON_ST_NONE;
	    goto more;

	case JSON_ST_BCOMMENT:
	JSON_ST_BCOMMENT:
	    while((ptr<ptr_end) && (*ptr != '*')) 
		ptr++;
	    if (ptr == ptr_end) goto swapout;
	    ptr++;
	    GOTO(JSON_ST_BSTAR);

	case JSON_ST_BSTAR:
	JSON_ST_BSTAR:
	    if (ptr < ptr_end) {
		if (*ptr == '/') {
		    ptr++;
		    state = JSON_ST_NONE;
		    goto more;
		}
		GOTO(JSON_ST_BCOMMENT);
	    }
	    goto swapout;

	case JSON_ST_STRING:
	JSON_ST_STRING:
	    while((ptr<ptr_end) && (*ptr != '"')) {
		if (*ptr == '\\') { ptr++; GOTO(JSON_ST_BQUOTE); }
		djb_hash_step(hash, *ptr);
		heap[hp++] = *ptr++;
		CHECK_HP(hp);
	    }
	    if (ptr == ptr_end) goto swapout;
	    ptr++;
	    heap[hp++] = '\0';
	    CHECK_HP(hp);	    
	    jp->current.s.len = (&heap[hp] - (uint8_t*)jp->current.s.ptr) - 1;
	    jp->current.s.key = json_find_key(jp, hash,
					      jp->current.s.ptr,
					      jp->current.s.len);
	    CALLBACK(hp);
	    goto next;

	case JSON_ST_BQUOTE: // backqoute inside "
	JSON_ST_BQUOTE:
	    if (ptr < ptr_end) {
		int c;
		switch(*ptr) {
		case '"': case '\\': case '/':
		    c=*ptr; break;
		case 'b': c='\b'; break;
		case 'f': c='\f'; break;
		case 'n': c='\n'; break;
		case 'r': c='\r'; break;
		case 't': c='\t'; break;
		case 'u': ptr++; GOTO(JSON_ST_U1);
		default: goto lex_error;
		}
		djb_hash_step(hash, c);
		heap[hp++] = c;
		ptr++;
		GOTO(JSON_ST_STRING);
	    }
	    goto swapout;

	case JSON_ST_U1:
	JSON_ST_U1:
	    // use heap[hp] to build char code (only bytes so far)
	    if (ptr < ptr_end) {
		int c = hex_digit(*ptr);
		if (c < 0) goto lex_error;
		heap[hp] = c;
		ptr++;
		GOTO(JSON_ST_U2);
	    }
	    goto swapout;

	case JSON_ST_U2:
	JSON_ST_U2:
	    if (ptr < ptr_end) {
		int c = hex_digit(*ptr);
		if (c < 0) goto lex_error;
		heap[hp] = (heap[hp]<<4)|c;
		ptr++;
		GOTO(JSON_ST_U3);
	    }
	    goto swapout;

	case JSON_ST_U3:
	JSON_ST_U3:
	    if (ptr < ptr_end) {
		int c = hex_digit(*ptr);
		if (c < 0) goto lex_error;
		heap[hp] = (heap[hp]<<4)|c;
		ptr++;
		GOTO(JSON_ST_U4);
	    }
	    goto swapout;

	case JSON_ST_U4:
	JSON_ST_U4:
	    if (ptr < ptr_end) {
		int c = hex_digit(*ptr);
		if (c < 0) goto lex_error;
		c = (heap[hp]<<4)|c;
		djb_hash_step(hash, c);
		heap[hp++] = c;
		ptr++;
		GOTO(JSON_ST_STRING);
	    }
	    goto swapout;

	case JSON_ST_IDENTIFIER:
	JSON_ST_IDENTIFIER:
	    while((ptr<ptr_end) &&
		  (isalpha(*ptr) || isdigit(*ptr) || 
		   (*ptr == '_') || (*ptr == '-'))) {
		djb_hash_step(hash, *ptr);
		heap[hp++] = *ptr++;
	    }
	    if (ptr == ptr_end)  // need an eof marker?
		goto swapout;
	    heap[hp++] = '\0';
	    jp->current.s.len = (&heap[hp] - (uint8_t*)jp->current.s.ptr) - 1;
	    if ((jp->h_true == hash) &&
		(strcmp((char*)jp->current.s.ptr, "true") == 0)) {
		hp -= (jp->current.s.len + 1);
		jp->current.type = JSON_TRUE;
	    }
	    else if ((jp->h_false == hash) &&
		     (strcmp((char*)jp->current.s.ptr, "false") == 0)) {
		hp -= (jp->current.s.len + 1);
		jp->current.type = JSON_FALSE;
	    }
	    else if ((jp->h_null == hash) &&
		     (strcmp((char*)jp->current.s.ptr, "null") == 0)) {
		hp -= (jp->current.s.len + 1);
		jp->current.type = JSON_NULL;
	    }
	    else {
		if (!jp->relaxed)
		    goto syntax_error;
		jp->current.s.key = json_find_key(jp, hash,
						  jp->current.s.ptr,
						  jp->current.s.len);
	    }
	    CALLBACK(hp);
	    goto next;

	case JSON_ST_MINUS:
	JSON_ST_MINUS:
	    if (ptr < ptr_end) {
		if (!isdigit(*ptr))
		    goto lex_error;
		GOTO(JSON_ST_INTEGER);
	    }
	    goto swapout;
	    
	case JSON_ST_INTEGER:
	JSON_ST_INTEGER: {
		int hp0;
		char* endptr;
		while((ptr < ptr_end) && isdigit(*ptr)) {
		    jp->heap[hp++] = *ptr++;
		}
		if (ptr == ptr_end)
		    goto swapout;
		if (*ptr == '.') {
		    jp->heap[hp++] = *ptr++;
		    jp->current.type = JSON_FLOAT;  // float processing
		    GOTO(JSON_ST_FRAC);
		}
		hp0 = jp->current.i;  // hold heap top
		jp->heap[hp++] = '\0';
		// printf("parse '%s'\n", (char*)&jp->heap[hp0]);
		jp->current.i = strtol((char*)&jp->heap[hp0],&endptr,10);
		if (!endptr || (*endptr != '\0'))
		    goto syntax_error;
		hp = hp0;
		CALLBACK(hp);
		goto next;
	    }

	case JSON_ST_FRAC:
	JSON_ST_FRAC: {
		int hp0;
		char* endptr;
		while((ptr < ptr_end) && isdigit(*ptr)) {
		    jp->heap[hp++] = *ptr++;
		}
		if (ptr == ptr_end)
		    goto swapout;
		if ((*ptr == 'e') || (*ptr == 'E')) {
		    jp->heap[hp++] = *ptr++;
		    GOTO(JSON_ST_E);
		}
		hp0 = jp->current.i;  // hold heap top
		jp->heap[hp++] = '\0';
		// printf("parse '%s'\n", (char*)&jp->heap[hp0]);
		jp->current.f = strtod((char*)&jp->heap[hp0],&endptr);
		if (!endptr || (*endptr != '\0'))
		    goto syntax_error;
		hp = hp0;
		CALLBACK(hp);
		goto next;
	    }

	case JSON_ST_E:
	JSON_ST_E: {
		if ((ptr < ptr_end) && ((*ptr=='+')||(*ptr=='-'))) {
		    jp->heap[hp++] = *ptr++;
		    GOTO(JSON_ST_ESIGN);
		}
		else if (ptr == ptr_end)
		    goto swapout;
		GOTO(JSON_ST_ESIGN);
	    }

	case JSON_ST_ESIGN:
	JSON_ST_ESIGN: {
		int hp0;
		char* endptr;
		while((ptr < ptr_end) && isdigit(*ptr)) {
		    jp->heap[hp++] = *ptr++;
		}
		if (ptr == ptr_end)
		    goto swapout;
		hp0 = jp->current.i;  // hold heap top
		jp->heap[hp++] = '\0';
		// printf("parse '%s'\n", (char*)&jp->heap[hp0]);
		jp->current.f = strtod((char*)&jp->heap[hp0],&endptr);
		if (!endptr || (*endptr != '\0'))
		    goto syntax_error;
		hp = hp0;
		CALLBACK(hp);
		goto next;
	    }

	default:
	    fprintf(stderr, "internal error json state undefined\n");
	    return -1;
	}
    }
    swapout:
    SWAPOUT(jp);
    return 0;

stack_overflow:
    fprintf(stderr, "%d: stack overflow\n", jp->line);
    return -1;

heap_overflow:
    fprintf(stderr, "%d: heap overflow\n", jp->line);
    return -1;

syntax_error:
    fprintf(stderr, "%d: syntax error\n", jp->line);
    return -1;    

lex_error:
    fprintf(stderr, "%d: unexpeced char '%c'\n", jp->line, *ptr);
    return -1;
}
