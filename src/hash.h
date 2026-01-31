// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef HASH_H_INCLUDED
#define HASH_H_INCLUDED

#include "core.h"
#include "object.h"

scm_obj_t hash_table_ref(scm_obj_t ht, scm_obj_t key, scm_obj_t default_value);
void hash_table_set(scm_obj_t ht, scm_obj_t key, scm_obj_t value);
void hash_table_delete(scm_obj_t ht, scm_obj_t key);

unsigned int string_hash(scm_obj_t obj, unsigned int bound);
bool string_equiv(scm_obj_t obj1, scm_obj_t obj2);
int find_hash_table_size(int nsize);

#endif