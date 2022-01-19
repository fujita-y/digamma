// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef SERIALIZE_H_INCLUDED
#define SERIALIZE_H_INCLUDED

#include "core.h"
#include "object.h"
#include "hash.h"

class object_heap_t;

class serializer_t {
  object_heap_t* m_heap;
  scm_hashtable_t m_tagged_datum;
  scm_obj_t* m_stack;
  scm_obj_t* m_stack_limit;
  scm_obj_t* m_sp;
  int m_buf_size;
  int m_buf_mark;
  uint8_t* m_buf;
  scm_obj_t m_bad;

  void emit_u8(uint8_t octet);
  void emit_u32(uint32_t n);
  void emit_u64(uint64_t n);
  void emit_bytes(const uint8_t* s, int n);
  void emit_uintptr(uintptr_t obj) {
#if ARCH_LP64
    emit_u64((uint64_t)obj);
#else
    emit_u32((uint32_t)obj);
#endif
  }
  void expand();
  void scan(scm_obj_t obj);
  void push(scm_obj_t obj);
  scm_obj_t pop();
  void put_tagged();
  void put_list(scm_obj_t obj);
  void put_datum(scm_obj_t obj);

 public:
  serializer_t(object_heap_t* heap);
  ~serializer_t();
  bool test(scm_obj_t obj);
  scm_obj_t translate(scm_obj_t obj);
};

class deserializer_t {
  object_heap_t* m_heap;
  scm_obj_t* m_tagged_datum;
  uint8_t* m_buf;
  uint8_t* m_buf_tail;

  uint8_t fetch_u8();
  uint32_t fetch_u32();
  uint64_t fetch_u64();
  void fetch_bytes(uint8_t* p, int n);
  uintptr_t fetch_uintptr() {
#if ARCH_LP64
    return fetch_u64();
#else
    return fetch_u32();
#endif
  }
  void get_tagged();
  scm_obj_t get_datum();
  void* fetch_closure_env();

 public:
  deserializer_t(object_heap_t* heap);
  ~deserializer_t();
  scm_obj_t translate(scm_bvector_t obj);
};

#endif
