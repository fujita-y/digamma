// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "hash.h"
#include "object_heap.h"

static inline int hash_busy_threshold(int nsize) { return nsize - (nsize >> 3); }         // 87.5%
static inline int hash_dense_threshold(int nsize) { return nsize - (nsize >> 2); }        // 75%
static inline int hash_sparse_threshold(int nsize) { return nsize >> 2; }                 // 25%
static inline int hash_renew_size(int live) { return live + (live >> 1) + (live >> 2); }  // 175%

static constexpr int primes[] = {7,         13,        29,        59,        113,        223,        431,       821,      1567,     2999,
                                 5701,      10837,     20593,     39133,     74353,      141277,     268439,    510047,   969097,   1841291,
                                 3498457,   5247701,   7871573,   11807381,  17711087,   26566649,   39849977,  59774983, 89662483, 134493731,
                                 201740597, 302610937, 453916423, 680874641, 1021311983, 1531968019, 2147483647};

int calc_hashtable_size(int nsize) {
  for (int i = 0; i < array_sizeof(primes); i++) {
    if (primes[i] > nsize) return primes[i];
  }
  fatal("%s:%u internal error: hashtable too big", __FILE__, __LINE__);
}

template <typename T> void swap(T& lhs, T& rhs) {
  T tmp = lhs;
  lhs = rhs;
  rhs = tmp;
}

static unsigned int second_hash(unsigned int hash, int nsize) {
  int dist = nsize >> 6;
  dist = (dist < 8) ? ((nsize > 8) ? 8 : 1) : dist;
  int hash2 = dist - (hash % dist);
  assert(hash2 > 0 && hash2 < nsize);
  return hash2;
}

unsigned int address_hash(scm_obj_t obj, unsigned int bound) { return (((uintptr_t)obj >> 3) * 2654435761U + ((uintptr_t)obj & 7)) % bound; }

bool address_equiv(scm_obj_t obj1, scm_obj_t obj2) { return obj1 == obj2; }

unsigned int string_hash(scm_obj_t obj, unsigned int bound) {
  assert(is_string(obj));
  unsigned int hash = 5381;
  const uint8_t* s = string_name(obj);
  int c;
  while ((c = *s++)) hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
  return hash % bound;
}

bool string_equiv(scm_obj_t obj1, scm_obj_t obj2) {
  assert(is_string(obj1));
  assert(is_string(obj2));
  return strcmp((const char*)string_name(obj1), (const char*)string_name(obj2)) == 0;
}

unsigned int symbol_hash(scm_obj_t obj, unsigned int bound) {
  assert(is_symbol(obj));
  return address_hash(obj, bound);
}

bool symbol_equiv(scm_obj_t obj1, scm_obj_t obj2) {
  assert(is_symbol(obj1));
  assert(is_symbol(obj2));
  return obj1 == obj2;
}

static scm_obj_t get(scm_hashtable_rec_t* ht, scm_obj_t key) {
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;
  int hash1 = (*ht->hash)(key, nsize);
  int hash2 = second_hash(hash1, nsize);
  int index = hash1;
  do {
    assert(index >= 0 && index < nsize);
    scm_obj_t tag = aux->elts[index];
    if (tag == scm_hash_free) return scm_undef;
    if (tag != scm_hash_deleted && (tag == key || (*ht->equiv)(tag, key))) return aux->elts[index + nsize];
    index += hash2;
    if (index >= nsize) index -= nsize;
  } while (index != hash1);
  return scm_undef;
}

static int put(scm_hashtable_rec_t* ht, scm_obj_t key, scm_obj_t value) {
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;
  int hash1 = (*ht->hash)(key, nsize);
  int hash2 = second_hash(hash1, nsize);
  int index = hash1;
  do {
    assert(index >= 0 && index < nsize);
    scm_obj_t tag = aux->elts[index];
    if (tag == scm_hash_free) {
      aux->live++;
      aux->used++;
      goto found;
    }
    if (tag == scm_hash_deleted) {
      aux->live++;
      goto found;
    }
    if (tag == key || (*ht->equiv)(tag, key)) goto found;
    index += hash2;
    if (index >= nsize) index -= nsize;
  } while (index != hash1);
  fatal("%s:%u hash table full.", __FILE__, __LINE__);

found:
  aux->elts[index] = key;
  aux->elts[index + nsize] = value;
  if (aux->used < hash_busy_threshold(nsize)) return 0;
  if (aux->live < hash_dense_threshold(nsize)) {
    if (aux->live < hash_sparse_threshold(aux->used)) {
      return calc_hashtable_size(hash_renew_size(aux->live));
    }
    return nsize;
  }
  return calc_hashtable_size(nsize);
}

static void remove(scm_hashtable_rec_t* ht, scm_obj_t key) {
  hashtable_aux_t* aux = ht->aux;
  int nsize = aux->capacity;
  int hash1 = (*ht->hash)(key, nsize);
  int hash2 = second_hash(hash1, nsize);
  int index = hash1;
  do {
    assert(index >= 0 && index < nsize);
    scm_obj_t tag = aux->elts[index];
    if (tag == scm_hash_free) return;
    if (tag != scm_hash_deleted && (tag == key || (*ht->equiv)(tag, key))) {
      aux->elts[index] = scm_hash_deleted;
      aux->elts[index + nsize] = scm_unspecified;
      aux->live--;
      return;
    }
    index += hash2;
    if (index >= nsize) index -= nsize;
  } while (index != hash1);
}

static void rehash(scm_hashtable_rec_t* ht, int nsize) {
  hashtable_aux_t* aux = ht->aux;
  int nelts = aux->capacity;
  scm_hashtable_rec_t* ht2 = (scm_hashtable_rec_t*)to_address(make_hashtable(ht->hash, ht->equiv, nsize));
  for (int i = 0; i < nelts; i++) {
    if (aux->elts[i] == scm_hash_free) continue;
    if (aux->elts[i] == scm_hash_deleted) continue;
    put(ht2, aux->elts[i], aux->elts[i + nelts]);
  }
  swap(ht->aux, ht2->aux);
}

scm_obj_t hashtable_ref(scm_obj_t obj, scm_obj_t key, scm_obj_t default_value) {
  assert(is_hashtable(obj));
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(obj);
  scm_obj_t value = get(ht, key);
  if (value != scm_undef) return value;
  return default_value;
}

void hashtable_set(scm_obj_t obj, scm_obj_t key, scm_obj_t value) {
  assert(is_hashtable(obj));
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(obj);
  scoped_lock lock(ht->lock);
  object_heap_t::current()->write_barrier(key);
  object_heap_t::current()->write_barrier(value);
  int n = put(ht, key, value);
  if (n) rehash(ht, n);
}

void hashtable_delete(scm_obj_t obj, scm_obj_t key) {
  assert(is_hashtable(obj));
  scm_hashtable_rec_t* ht = (scm_hashtable_rec_t*)to_address(obj);
  scoped_lock lock(ht->lock);
  remove(ht, key);
}
