// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef HASH_H_INCLUDED
#define HASH_H_INCLUDED

#include "core.h"
#include "object.h"

scm_obj_t hashtable_ref(scm_obj_t ht, scm_obj_t key, scm_obj_t default_value);
void hashtable_set(scm_obj_t ht, scm_obj_t key, scm_obj_t value);
void hashtable_delete(scm_obj_t ht, scm_obj_t key);
int calc_hashtable_size(int nsize);

unsigned int string_hash(scm_obj_t obj, unsigned int bound);
bool string_equiv(scm_obj_t obj1, scm_obj_t obj2);
unsigned int symbol_hash(scm_obj_t obj, unsigned int bound);
bool symbol_equiv(scm_obj_t obj1, scm_obj_t obj2);
unsigned int address_hash(scm_obj_t obj, unsigned int bound);
bool address_equiv(scm_obj_t obj1, scm_obj_t obj2);

#endif