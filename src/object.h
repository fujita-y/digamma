// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_H_INCLUDED
#define OBJECT_H_INCLUDED

#include "core.h"
#include "mutex.h"
#include "queue.h"

/*
    immediate:

    nnnn nnnn nnnn nnnn nnnn nnnn nnnn nnn1 : scm_fixnum_t
    pppp pppp pppp pppp pppp pppp pppp p000 : scm_cell_t
    cccc cccc cccc cccc cccc cccc 0000 0010 : scm_char_t
    0000 0000 0000 0000 0000 0000 0001 0010 : scm_true
    0000 0000 0000 0000 0000 0000 0010 0010 : scm_false
    0000 0000 0000 0000 0000 0000 0011 0010 : scm_nil
    0000 0000 0000 0000 0000 0000 0100 0010 : scm_undef
    0000 0000 0000 0000 0000 0000 0101 0010 : scm_unspecified
    0000 0000 0000 0000 0000 0000 0110 0010 : scm_eof
    0000 0000 0000 0000 0000 0000 0111 0010 : scm_hash_free
    0000 0000 0000 0000 0000 0000 1000 0010 : scm_hash_used
    0000 0000 0000 0000 0000 0000 1001 0010 : scm_proc_apply
    0000 0000 0000 0000 0000 0000 1010 0010 : scm_proc_callcc
    0000 0000 0000 0000 0000 0000 1011 0010 : scm_proc_apply_values
    .... .... .... .... .... .... 1101 0010 : (reserved)
    .... .... .... .... .... .... 1110 0010 : (reserved)
    .... .... .... .... .... .... 1111 0010 : (reserved)

    pppp pppp pppp pppp pppp pppp pppp p110 : heap forward pointer

    boxed:

    nnnn nnnn nnnn SSSS SSSS IU-- ---- 1010 : scm_hdr_symbol        I: inherent U: uninterned S: symbol code
    .... .... .... .... TTTT L.-- ---- 1010 : scm_hdr_string        L: literal T: (0x0 unknown) (0x1 ascii) (0x2 utf8)
    nnnn nnnn nnnn nnnn .... NZ-- ---- 1010 : scm_hdr_bignum        NZ: (01 positive) (11 negative) (00 zero)
    .... .... .... .... .... P.-- ---- 1010 : scm_hdr_flonum        P: precision (0 64bit) (1 32bit)
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_cont
    nnnn nnnn nnnn nnnn .... C.-- ---- 1010 : scm_hdr_closure       I: compiled, n == (- 1 - <required argc>) if there are rest arguments
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_subr
    .... .... .... .... .... L.-- ---- 1010 : scm_hdr_vector        L: literal
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_port
    nnnn nnnn nnnn nnnn .... ..-- ---- 1010 : scm_hdr_values
    .... .... .... .... .... IU-- ---- 1010 : scm_hdr_hashtable     I: immutable U: unsafe
    .... .... .... .... .... U.-- ---- 1010 : scm_hdr_gloc          U: uninterned
    nnnn nnnn nnnn nnnn .... ..-- ---- 1010 : scm_hdr_tuple
    .... .... .... .... .... IU-- ---- 1010 : scm_hdr_weakhashtable I: immutable U: unsafe
    .... .... .... .... .... LM-- ---- 1010 : scm_hdr_bvector       L: literal M: mapped
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_complex
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_rational
    nnnn nnnn nnnn nnnn .... ..-- ---- 1010 : scm_hdr_heapenv
    nnnn nnnn nnnn nnnn .... ..-- ---- 1010 : scm_hdr_heapcont
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_weakmapping
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_environment
    .... .... .... .... .... ..-- ---- 1010 : scm_hdr_socket
*/

#define OBJECT_DATUM_ALIGN      8
#define OBJECT_DATUM_ALIGN_MASK (OBJECT_DATUM_ALIGN - 1)

#define PORT_LOOKAHEAD_SIZE     6

typedef void* scm_obj_t;
typedef uintptr_t scm_hdr_t;
typedef scm_obj_t scm_cell_t;
typedef scm_obj_t scm_char_t;
typedef scm_obj_t scm_fixnum_t;

#if ARCH_LP64
  #define USE_DIGIT32 0
  #define USE_DIGIT64 1
typedef uint64_t digit_t;
#else
  #define USE_DIGIT32 1
  #define USE_DIGIT64 0
typedef uint32_t digit_t;
#endif

const scm_obj_t scm_true = (scm_obj_t)0x12;
const scm_obj_t scm_false = (scm_obj_t)0x22;
const scm_obj_t scm_nil = (scm_obj_t)0x32;
const scm_obj_t scm_undef = (scm_obj_t)0x42;  // unbound variable
const scm_obj_t scm_unspecified = (scm_obj_t)0x52;
const scm_obj_t scm_eof = (scm_obj_t)0x62;
const scm_obj_t scm_timeout = (scm_obj_t)0x72;
const scm_obj_t scm_shutdown = (scm_obj_t)0x82;
const scm_obj_t scm_hash_free = (scm_obj_t)0x92;     // internal use
const scm_obj_t scm_hash_deleted = (scm_obj_t)0xa2;  // internal use
const scm_obj_t scm_proc_apply = (scm_obj_t)0xb2;
const scm_obj_t scm_proc_callcc = (scm_obj_t)0xc2;
const scm_obj_t scm_proc_apply_values = (scm_obj_t)0xd2;

// primitive
#define TC_FLONUM         0x00
#define TC_BVECTOR        0x01
// finalize only
#define TC_BIGNUM         0x02
#define TC_SYMBOL         0x03
#define TC_STRING         0x04
// finalize & trace
#define TC_VECTOR         0x05
#define TC_TUPLE          0x06
#define TC_VALUES         0x07
#define TC_HASHTABLE      0x08
#define TC_WEAKHASHTABLE  0x09
#define TC_PORT           0x0a
// trace only (1)
#define TC_CLOSURE        0x0b
#define TC_CONT           0x0c
#define TC_GLOC           0x0d
#define TC_SUBR           0x0e
// trace only (2)
#define TC_COMPLEX        0x0f
#define TC_RATIONAL       0x10
#define TC_HEAPENV        0x11
#define TC_HEAPCONT       0x12
#define TC_WEAKMAPPING    0x13
#define TC_ENVIRONMENT    0x14
#define TC_SOCKET         0x15
#define TC_MASKBITS       0x3f

#define HDR_ATTR_BOXED    0xa
#define HDR_ATTR_MASKBITS 0xf

#define HDR_TYPE_MASKBITS 0x3ff

const scm_hdr_t scm_hdr_symbol = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_SYMBOL << 4));
const scm_hdr_t scm_hdr_string = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_STRING << 4));
const scm_hdr_t scm_hdr_bignum = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_BIGNUM << 4));
const scm_hdr_t scm_hdr_flonum = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_FLONUM << 4));
const scm_hdr_t scm_hdr_cont = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_CONT << 4));
const scm_hdr_t scm_hdr_closure = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_CLOSURE << 4));
const scm_hdr_t scm_hdr_subr = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_SUBR << 4));
const scm_hdr_t scm_hdr_vector = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_VECTOR << 4));
const scm_hdr_t scm_hdr_port = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_PORT << 4));
const scm_hdr_t scm_hdr_values = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_VALUES << 4));
const scm_hdr_t scm_hdr_hashtable = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_HASHTABLE << 4));
const scm_hdr_t scm_hdr_complex = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_COMPLEX << 4));
const scm_hdr_t scm_hdr_rational = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_RATIONAL << 4));
const scm_hdr_t scm_hdr_heapenv = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_HEAPENV << 4));
const scm_hdr_t scm_hdr_heapcont = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_HEAPCONT << 4));
const scm_hdr_t scm_hdr_environment = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_ENVIRONMENT << 4));
const scm_hdr_t scm_hdr_gloc = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_GLOC << 4));
const scm_hdr_t scm_hdr_tuple = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_TUPLE << 4));
const scm_hdr_t scm_hdr_weakmapping = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_WEAKMAPPING << 4));
const scm_hdr_t scm_hdr_weakhashtable = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_WEAKHASHTABLE << 4));
const scm_hdr_t scm_hdr_bvector = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_BVECTOR << 4));
const scm_hdr_t scm_hdr_socket = HDR_TYPE_MASKBITS & (HDR_ATTR_BOXED | (TC_SOCKET << 4));

struct scm_pair_rec_t;
struct scm_symbol_rec_t;
struct scm_string_rec_t;
struct scm_flonum_rec_t;
struct scm_cont_rec_t;
struct scm_closure_rec_t;
struct scm_subr_rec_t;
struct scm_vector_rec_t;
struct scm_port_rec_t;
struct scm_values_rec_t;
struct scm_hashtable_rec_t;
struct scm_bignum_rec_t;
struct scm_complex_rec_t;
struct scm_rational_rec_t;
struct scm_environment_rec_t;
struct scm_gloc_rec_t;
struct scm_tuple_rec_t;
struct scm_weakmapping_rec_t;
struct scm_weakhashtable_rec_t;
struct scm_bvector_rec_t;
struct scm_socket_rec_t;

typedef scm_pair_rec_t* scm_pair_t;
typedef scm_symbol_rec_t* scm_symbol_t;
typedef scm_string_rec_t* scm_string_t;
typedef scm_flonum_rec_t* scm_flonum_t;
typedef scm_cont_rec_t* scm_cont_t;
typedef scm_closure_rec_t* scm_closure_t;
typedef scm_subr_rec_t* scm_subr_t;
typedef scm_vector_rec_t* scm_vector_t;
typedef scm_port_rec_t* scm_port_t;
typedef scm_values_rec_t* scm_values_t;
typedef scm_hashtable_rec_t* scm_hashtable_t;
typedef scm_bignum_rec_t* scm_bignum_t;
typedef scm_complex_rec_t* scm_complex_t;
typedef scm_rational_rec_t* scm_rational_t;
typedef scm_environment_rec_t* scm_environment_t;
typedef scm_gloc_rec_t* scm_gloc_t;
typedef scm_tuple_rec_t* scm_tuple_t;
typedef scm_weakmapping_rec_t* scm_weakmapping_t;
typedef scm_weakhashtable_rec_t* scm_weakhashtable_t;
typedef scm_bvector_rec_t* scm_bvector_t;
typedef scm_socket_rec_t* scm_socket_t;

struct vm_cont_rec_t;
struct vm_env_rec_t;

typedef vm_cont_rec_t* vm_cont_t;
typedef vm_env_rec_t* vm_env_t;

typedef scm_obj_t (*subr_proc_t)(VM*, int argc, scm_obj_t argv[]);
typedef uint32_t (*hash_proc_t)(scm_obj_t obj, uint32_t bound);
typedef bool (*equiv_proc_t)(scm_obj_t obj1, scm_obj_t obj2);

#define OBJECT_ALIGNED(x) struct DECLSPEC(align(OBJECT_DATUM_ALIGN)) x
#define END               ATTRIBUTE(aligned(OBJECT_DATUM_ALIGN))

OBJECT_ALIGNED(scm_pair_rec_t) {
  scm_obj_t car;
  scm_obj_t cdr;
}
END;

OBJECT_ALIGNED(scm_symbol_rec_t) {
  scm_hdr_t hdr;
  char* name;  // uninterned symbol contains <prefix-size> after '\0'
}
END;

OBJECT_ALIGNED(scm_string_rec_t) {
  scm_hdr_t hdr;
  int size;
  char* name;
}
END;

OBJECT_ALIGNED(scm_flonum_rec_t) {
  scm_hdr_t hdr;
  double value;
}
END;

OBJECT_ALIGNED(scm_cont_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t wind_rec;
  void* cont;
}
END;

OBJECT_ALIGNED(scm_closure_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t doc;
  void* env;
  void* code;
  scm_obj_t pc;
}
END;

OBJECT_ALIGNED(scm_subr_rec_t) {
  scm_hdr_t hdr;
  subr_proc_t adrs;
  scm_obj_t doc;
#if PROFILE_SUBR
  uint64_t c_push;
  uint64_t c_load;
  uint64_t c_apply;
#endif
}
END;

OBJECT_ALIGNED(scm_vector_rec_t) {
  scm_hdr_t hdr;
  int count;
  scm_obj_t* elts;
}
END;

OBJECT_ALIGNED(scm_values_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t* elts;
}
END;

OBJECT_ALIGNED(scm_tuple_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t* elts;
}
END;

OBJECT_ALIGNED(scm_bvector_rec_t) {
  scm_hdr_t hdr;
  int count;
  uint8_t* elts;
}
END;

OBJECT_ALIGNED(scm_weakmapping_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t key;
  scm_obj_t value;
}
END;

OBJECT_ALIGNED(scm_port_rec_t) {
  scm_hdr_t hdr;
  mutex_t lock;
  scm_obj_t handlers;
  scm_obj_t bytes;
  uint8_t lookahead[PORT_LOOKAHEAD_SIZE];
  int lookahead_size;
  uint8_t* buf;
  uint8_t* buf_head;
  uint8_t* buf_tail;
  int buf_size;
  int buf_state;
  off64_t mark;
  int line;
  int column;
  fd_t fd;
  scm_obj_t name;
  scm_obj_t transcoder;
  uint8_t codec;
  uint8_t eol_style;
  uint8_t error_handling_mode;
  uint8_t file_options;
  uint8_t buffer_mode;
  uint8_t type;
  uint8_t subtype;
  uint8_t direction;
  bool force_sync;
  bool bom_le;
  bool bom_be;
  bool track_line_column;
  bool opened;
}
END;

OBJECT_ALIGNED(hashtable_rec_t) {
  int capacity;
  int used;
  int live;
  scm_obj_t elts[1];  // [ key ... val ... ]
}
END;

OBJECT_ALIGNED(scm_hashtable_rec_t) {
  scm_hdr_t hdr;
  mutex_t lock;
  int type;
  hash_proc_t hash;
  equiv_proc_t equiv;
  hashtable_rec_t* datum;  // [ key ... val ... ]
  scm_obj_t handlers;
}
END;

OBJECT_ALIGNED(weakhashtable_rec_t) {
  int capacity;
  int used;
  int live;
  scm_obj_t elts[1];  // [ key ... val ... ]
}
END;

OBJECT_ALIGNED(scm_weakhashtable_rec_t) {
  scm_hdr_t hdr;
  mutex_t lock;
  weakhashtable_rec_t* datum;  // [ weak-mapping ... ]
}
END;

OBJECT_ALIGNED(scm_bignum_rec_t) {
  scm_hdr_t hdr;
  digit_t* elts;
}
END;

OBJECT_ALIGNED(scm_complex_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t imag;
  scm_obj_t real;
}
END;

OBJECT_ALIGNED(scm_rational_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t nume;
  scm_obj_t deno;
}
END;

OBJECT_ALIGNED(scm_environment_rec_t) {
  scm_hdr_t hdr;
  scm_hashtable_t variable;  // key:symbol value:gloc
  scm_hashtable_t macro;
  scm_string_t name;
}
END;

OBJECT_ALIGNED(scm_gloc_rec_t) {
  scm_hdr_t hdr;
  scm_obj_t value;
  scm_obj_t variable;  // for error message
}
END;

OBJECT_ALIGNED(scm_socket_rec_t) {
  scm_hdr_t hdr;
  int mode;
  int fd;
  int family;
  int socktype;
  int protocol;
  int addrlen;
  struct sockaddr_storage addr;
}
END;

#undef OBJECT_ALIGNED
#undef END

struct vm_cont_rec_t {
  // scm_obj_t args[argc];
  scm_obj_t pc;
  scm_obj_t trace;
  scm_obj_t* fp;
  void* env;
  void* code;
  void* up;  // 'm_cont' and 'up' point here
};

struct vm_env_rec_t {
  // scm_obj_t vars[count];
  intptr_t count;
  void* up;  // 'm_env' and 'up' point here
};

#define HEAPFORWARDPTR(obj)     ((intptr_t)(obj) & (~0x7))
#define HEAPFORWARDPTRP(obj)    (((intptr_t)(obj)&0x7) == 0x6)
#define MAKEHEAPFORWARDPTR(obj) ((intptr_t)(obj) | 0x6)

#define FIXNUM_MAX              (INTPTR_MAX / 2)
#define FIXNUM_MIN              (INTPTR_MIN / 2)
#if ARCH_LP64
  #define FIXNUM_BITS (64 - 1)
#else
  #define FIXNUM_BITS (32 - 1)
#endif

#define FIXNUM(obj)                       ((intptr_t)(obj) >> 1)
#define CHAR(obj)                         ((uintptr_t)(obj) >> 8)
#define FLONUM(obj)                       (((scm_flonum_t)obj)->value)
#define BITS(obj)                         ((uintptr_t)(obj))
#define HDR(obj)                          (*(scm_hdr_t*)(obj))

#define BOOLP(obj)                        (((obj) == scm_true) | ((obj) == scm_false))
#define FIXNUMP(obj)                      ((BITS(obj) & 0x1))
#define CELLP(obj)                        ((BITS(obj) & 0x7) == 0)
#define CHARP(obj)                        ((BITS(obj) & 0xff) == 0x02)
#define PAIRP(obj)                        (CELLP(obj) && (HDR(obj) & HDR_ATTR_MASKBITS) != HDR_ATTR_BOXED)
#define FLONUMP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_flonum)
#define BVECTORP(obj)                     (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_bvector)
#define BIGNUMP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_bignum)
#define SYMBOLP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_symbol)
#define STRINGP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_string)
#define VECTORP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_vector)
#define TUPLEP(obj)                       (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_tuple)
#define VALUESP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_values)
#define HASHTABLEP(obj)                   (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_hashtable)
#define WEAKHASHTABLEP(obj)               (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_weakhashtable)
#define PORTP(obj)                        (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_port)
#define CLOSUREP(obj)                     (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_closure)
#define CONTP(obj)                        (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_cont)
#define GLOCP(obj)                        (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_gloc)
#define SUBRP(obj)                        (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_subr)
#define COMPLEXP(obj)                     (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_complex)
#define RATIONALP(obj)                    (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_rational)
#define HEAPENVP(obj)                     (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_heapenv)
#define HEAPCONTP(obj)                    (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_heapcont)
#define WEAKMAPPINGP(obj)                 (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_weakmapping)
#define ENVIRONMENTP(obj)                 (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_environment)
#define SOCKETP(obj)                      (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_socket)
#define SHAREDQUEUEP(obj)                 (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_sharedqueue)
#define SHAREDBAGP(obj)                   (CELLP(obj) && (HDR(obj) & HDR_TYPE_MASKBITS) == scm_hdr_sharedbag)

#define HDR_SYMBOL_INHERENT_SHIFT         11
#define HDR_SYMBOL_UNINTERNED_SHIFT       10
#define HDR_SYMBOL_CODE_SHIFT             12
#define HDR_SYMBOL_SIZE_SHIFT             20
#define HDR_STRING_LITERAL_SHIFT          11
#define HDR_STRING_TYPE_SHIFT             12
#define HDR_VALUES_COUNT_SHIFT            16
#define HDR_TUPLE_COUNT_SHIFT             16
#define HDR_HEAPENV_SIZE_SHIFT            16
#define HDR_HEAPCONT_SIZE_SHIFT           16
#define HDR_HASHTABLE_SHARED_SHIFT        10
#define HDR_HASHTABLE_IMMUTABLE_SHIFT     11
#define HDR_WEAKHASHTABLE_SHARED_SHIFT    10
#define HDR_WEAKHASHTABLE_IMMUTABLE_SHIFT 11
#define HDR_BIGNUM_SIGN_SHIFT             10
#define HDR_BIGNUM_COUNT_SHIFT            16
#define HDR_CLOSURE_ARGS_SHIFT            16
#define HDR_FLONUM_32BIT_SHIFT            11
#define HDR_VECTOR_LITERAL_SHIFT          11
#define HDR_BVECTOR_MAPPING_SHIFT         10
#define HDR_BVECTOR_LITERAL_SHIFT         11
#define HDR_BVECTOR_PINNED_SHIFT          12
#define HDR_GLOC_UNINTERNED_SHIFT         11
#define HDR_CLOSURE_INSPECTED_SHIFT       11

#define HDR_TC(hdr)                       (((hdr) >> 4) & TC_MASKBITS)
#define HDR_CLOSURE_ARGS(hdr)             (((intptr_t)(hdr)) >> HDR_CLOSURE_ARGS_SHIFT)
#define HDR_CLOSURE_INSPECTED(hdr)        (((hdr) >> HDR_CLOSURE_INSPECTED_SHIFT) & 1)
#define HDR_STRING_LITERAL(hdr)           (((hdr) >> HDR_STRING_LITERAL_SHIFT) & 1)
#define HDR_STRING_TYPE(hdr)              (((hdr) >> HDR_STRING_TYPE_SHIFT) & 1)
#define HDR_VALUES_COUNT(hdr)             (((uintptr_t)(hdr)) >> HDR_VALUES_COUNT_SHIFT)
#define HDR_TUPLE_COUNT(hdr)              (((uintptr_t)(hdr)) >> HDR_TUPLE_COUNT_SHIFT)
#define HDR_HEAPENV_SIZE(hdr)             (((uintptr_t)(hdr)) >> HDR_HEAPENV_SIZE_SHIFT)
#define HDR_HEAPCONT_SIZE(hdr)            (((uintptr_t)(hdr)) >> HDR_HEAPCONT_SIZE_SHIFT)
#define HDR_BIGNUM_COUNT(hdr)             (((uintptr_t)(hdr)) >> HDR_BIGNUM_COUNT_SHIFT)
#define HDR_SYMBOL_SIZE(hdr)              (((uintptr_t)(hdr)) >> HDR_SYMBOL_SIZE_SHIFT)
#define HDR_SYMBOL_CODE(hdr)              (((hdr) >> HDR_SYMBOL_CODE_SHIFT) & 0xff)
#define HDR_HASHTABLE_SHARED(hdr)         (((hdr) >> HDR_HASHTABLE_SHARED_SHIFT) & 1)
#define HDR_HASHTABLE_IMMUTABLE(hdr)      (((hdr) >> HDR_HASHTABLE_IMMUTABLE_SHIFT) & 1)
#define HDR_WEAKHASHTABLE_SHARED(hdr)     (((hdr) >> HDR_WEAKHASHTABLE_SHARED_SHIFT) & 1)
#define HDR_WEAKHASHTABLE_IMMUTABLE(hdr)  (((hdr) >> HDR_WEAKHASHTABLE_IMMUTABLE_SHIFT) & 1)
#define HDR_BIGNUM_SIGN(hdr)              (((hdr) >> HDR_BIGNUM_SIGN_SHIFT) & 0x03)
#define HDR_FLONUM_32BIT(hdr)             (((hdr) >> HDR_FLONUM_32BIT_SHIFT) & 1)
#define HDR_VECTOR_LITERAL(hdr)           (((hdr) >> HDR_VECTOR_LITERAL_SHIFT) & 1)
#define HDR_BVECTOR_MAPPING(hdr)          (((hdr) >> HDR_BVECTOR_MAPPING_SHIFT) & 1)
#define HDR_BVECTOR_LITERAL(hdr)          (((hdr) >> HDR_BVECTOR_LITERAL_SHIFT) & 1)
#define HDR_BVECTOR_PINNED(hdr)           (((hdr) >> HDR_BVECTOR_PINNED_SHIFT) & 1)

#define HDR_SYMBOL_INHERENT_BIT           ((uintptr_t)1 << HDR_SYMBOL_INHERENT_SHIFT)
#define HDR_SYMBOL_UNINTERNED_BIT         ((uintptr_t)1 << HDR_SYMBOL_UNINTERNED_SHIFT)
#define HDR_GLOC_UNINTERNED_BIT           ((uintptr_t)1 << HDR_GLOC_UNINTERNED_SHIFT)

#define INHERENTSYMBOLP(obj)              (CELLP(obj) && ((HDR(obj) & 0xfff) == (scm_hdr_symbol | HDR_SYMBOL_INHERENT_BIT)))
#define UNINTERNEDSYMBOLP(obj)            (CELLP(obj) && ((HDR(obj) & 0xfff) == (scm_hdr_symbol | HDR_SYMBOL_UNINTERNED_BIT)))
#define UNINTERNEDGLOCP(obj)              (CELLP(obj) && ((HDR(obj) & 0xfff) == (scm_hdr_gloc | HDR_GLOC_UNINTERNED_BIT)))
#define OPCODESYMBOLP(obj)                (INHERENTSYMBOLP(obj) && (HDR_SYMBOL_CODE(HDR(obj)) < VMOP_INSTRUCTION_COUNT))
#define BOTHFLONUMP(x, y) \
  (CELLP((intptr_t)(x) | (intptr_t)(y)) && ((((scm_flonum_t)(x))->hdr == scm_hdr_flonum) & (((scm_flonum_t)(y))->hdr == scm_hdr_flonum)))
#define BVECTORMAPPINGP(obj)     (BVECTORP(obj) && HDR_BVECTOR_MAPPING(HDR(obj)))

#define STRING_TYPE_UNKNOWN      0x0
#define STRING_TYPE_ASCII        0x1
#define STRING_TYPE_UTF8         0x2

#define CAR(obj)                 (((scm_pair_t)(obj))->car)
#define CDR(obj)                 (((scm_pair_t)(obj))->cdr)
#define CAAR(obj)                (CAR(CAR(obj)))
#define CADR(obj)                (CAR(CDR(obj)))
#define CDAR(obj)                (CDR(CAR(obj)))
#define CDDR(obj)                (CDR(CDR(obj)))
#define CADDR(obj)               (CAR(CDR(CDR(obj))))
#define CADAR(obj)               (CAR(CDR(CAR(obj))))
#define CDDDR(obj)               (CDR(CDR(CDR(obj))))

#define MAKEFIXNUM(n)            ((scm_fixnum_t)(((uintptr_t)(n) << 1) + 1))
#define MAKECHAR(n)              ((scm_char_t)(((uintptr_t)(n) << 8) + 0x02))
#define MAKEBITS(n, shift)       (((uintptr_t)(n)) << shift)

#define HASH_BUSY_THRESHOLD(n)   ((n) - ((n) >> 3))               // 87.5%
#define HASH_DENSE_THRESHOLD(n)  ((n) - ((n) >> 2))               // 75%
#define HASH_SPARSE_THRESHOLD(n) ((n) >> 2)                       // 25%
#define HASH_IMMUTABLE_SIZE(n)   ((n) + ((n) >> 3))               // 112.5%
#define HASH_MUTABLE_SIZE(n)     ((n) + ((n) >> 1) + ((n) >> 2))  // 175%
#define HASH_BOUND_MAX           UINT32_MAX

#if ARCH_LP64
  #define OBJECT_SLAB_SIZE       (8192L)
  #define OBJECT_SLAB_SIZE_SHIFT 13
  #define OBJECT_SLAB_THRESHOLD  (OBJECT_SLAB_SIZE / 8)  // m_shared[] and m_atomic[] in ObjectFactory in effect this value
  #define VM_STACK_BYTESIZE      OBJECT_SLAB_SIZE        // (OBJECT_SLAB_SIZE * 2)
#else
  #define OBJECT_SLAB_SIZE       (4096L)
  #define OBJECT_SLAB_SIZE_SHIFT 12
  #define OBJECT_SLAB_THRESHOLD  (OBJECT_SLAB_SIZE / 4)  // m_shared[] and m_atomic[] in ObjectFactory in effect this value
  #define VM_STACK_BYTESIZE      OBJECT_SLAB_SIZE        // (OBJECT_SLAB_SIZE * 2)
#endif
#define VM_STACK_BUSY_THRESHOLD(n)     ((n) - ((n) >> 2))  // 75%

#define IDENTIFIER_RENAME_DELIMITER    '`'
#define IDENTIFIER_LIBRARY_SUFFIX      '\''
#define IDENTIFIER_LIBRARY_INFIX       '.'
#define IDENTIFIER_PRIMITIVE_PREFIX    '.'
#define IDENTIFIER_TEMPORARY_DELIMITER '~'
#define IDENTIFIER_CSTUB_MARK          '@'

#define READ_STRING_SMALL_BUFFER_SIZE  2048
#define MAX_READ_SYMBOL_LENGTH         256
#define MAX_SOURCE_COLUMN              1024

struct reader_exception_t {
  scm_string_t m_message;
  reader_exception_t(scm_string_t message) { m_message = message; }
};

struct io_exception_t {
  int m_operation;
  int m_err;
  const char* m_message;
  io_exception_t(int opration, int err) {
    m_operation = opration;
    m_err = err;
    m_message = strerror(err);
  }
  io_exception_t(int opration, const char* message) {
    m_operation = opration;
    m_err = 0;
    m_message = message;
  }
};

struct io_codec_exception_t {
  int m_operation;
  scm_obj_t m_ch;
  const char* m_message;
  io_codec_exception_t(int opration, const char* message, scm_obj_t ch) {
    m_operation = opration;
    m_ch = ch;
    m_message = message;
  }
};

struct vm_exception_t {
  vm_exception_t() {}
};

struct vm_escape_t {
  vm_escape_t() {}
};

struct vm_continue_t {
  vm_continue_t() {}
};

inline const char* get_tuple_type_name(scm_obj_t obj) {
  if (TUPLEP(obj)) {
    scm_tuple_t tuple = (scm_tuple_t)obj;
    scm_obj_t e0 = tuple->elts[0];
    if (SYMBOLP(e0)) {
      scm_symbol_t type = (scm_symbol_t)e0;
      if (strncmp(type->name, "type:", 5) == 0) return type->name + 5;
    }
  }
  return NULL;
}

#endif
