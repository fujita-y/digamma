#include "../src/core.h"
#include "../src/hash.h"
#include "../src/object.h"
#include "../src/object_heap.h"
#include "../src/context.h"

void fatal(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void warning(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
}

void trace(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}


static bool some_test_failed = false;


static bool test_hashtable() {
  int capacity = 7;
  scm_obj_t ht = make_hashtable(string_hash, string_equiv, capacity);

  // Test basic set/get
  scm_obj_t key1 = make_string("foo");
  scm_obj_t val1 = make_fixnum(100);
  hashtable_set(ht, key1, val1);

  scm_obj_t got1 = hashtable_ref(ht, key1, scm_undef);
  if (got1 != val1) {
    printf("\033[31m###### hashtable failed: basic set/get\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test delete
  hashtable_delete(ht, key1);
  scm_obj_t got3 = hashtable_ref(ht, key1, scm_undef);
  if (got3 != scm_undef) {
    printf("\033[31m###### hashtable failed: delete\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test delete non-existent key
  scm_obj_t key_missing = make_string("non-existent");
  hashtable_delete(ht, key_missing);  // should not crash or affect other items

  // Test default value
  scm_obj_t key2 = make_string("bar");
  scm_obj_t def = make_fixnum(999);
  scm_obj_t got2 = hashtable_ref(ht, key2, def);
  if (got2 != def) {
    printf("\033[31m###### hashtable failed: default value\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test rehash (add more items than capacity)
  for (int i = 0; i < 20; i++) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    hashtable_set(ht, make_string(buf), make_fixnum(i));
  }

  // Delete some items from the expanded table
  for (int i = 0; i < 20; i += 2) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    hashtable_delete(ht, make_string(buf));
  }

  for (int i = 0; i < 20; i++) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    scm_obj_t val = hashtable_ref(ht, make_string(buf), scm_undef);
    if (i % 2 == 0) {
      if (val != scm_undef) {
        printf("\033[31m###### hashtable failed: delete verification at index %d\033[0m\n", i);
        some_test_failed = true;
        return false;
      }
    } else {
      if (fixnum(val) != i) {
        printf("\033[31m###### hashtable failed: rehash verification at index %d\033[0m\n", i);
        some_test_failed = true;
        return false;
      }
    }
  }

  printf("\033[32mhashtable passed\033[0m\n");
  return true;
}

static bool test_equal_hash() {
  // Test symbol hash stability
  scm_obj_t sym1 = make_symbol("foo");
  scm_obj_t sym2 = make_symbol("foo");
  if (equal_hash(sym1, INT32_MAX) != equal_hash(sym2, INT32_MAX)) {
    printf("\033[31m###### equal_hash failed: symbol stability\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test string hash stability
  scm_obj_t str1 = make_string("bar");
  scm_obj_t str2 = make_string("bar");
  if (equal_hash(str1, INT32_MAX) != equal_hash(str2, INT32_MAX)) {
    printf("\033[31m###### equal_hash failed: string stability\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test list recursion
  scm_obj_t list1 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  scm_obj_t list2 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  if (equal_hash(list1, INT32_MAX) != equal_hash(list2, INT32_MAX)) {
    printf("\033[31m###### equal_hash failed: list recursion\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test vector recursion
  scm_obj_t vec1 = make_vector(3, make_fixnum(0));
  scm_obj_t* elts1 = vector_elts(vec1);
  elts1[0] = make_fixnum(1);
  elts1[1] = make_fixnum(2);
  elts1[2] = make_fixnum(3);

  scm_obj_t vec2 = make_vector(3, make_fixnum(0));
  scm_obj_t* elts2 = vector_elts(vec2);
  elts2[0] = make_fixnum(1);
  elts2[1] = make_fixnum(2);
  elts2[2] = make_fixnum(3);

  if (equal_hash(vec1, INT32_MAX) != equal_hash(vec2, INT32_MAX)) {
    printf("\033[31m###### equal_hash failed: vector recursion\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test flonum hash (eqv_hash)
  scm_obj_t f1 = make_flonum(3.14);
  scm_obj_t f2 = make_flonum(3.14);
  if (eqv_hash(f1, INT32_MAX) != eqv_hash(f2, INT32_MAX)) {
    printf("\033[31m###### eqv_hash failed: flonum stability\033[0m\n");
    some_test_failed = true;
    return false;
  }

  printf("\033[32mequal_hash passed\033[0m\n");
  return true;
}

static bool test_regression() {
  // Test fixnum eqv_hash
  if (eqv_hash(make_fixnum(123), INT32_MAX) != eqv_hash(make_fixnum(123), INT32_MAX)) {
    printf("\033[31m###### eqv_hash failed: fixnum stability\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (eqv_hash(make_fixnum(123), INT32_MAX) == eqv_hash(make_fixnum(124), INT32_MAX)) {
    // collision check, probabilistic but very unlikely for simple fixnums in large space
    // actually for fixnums (pointer based) hash might be just shifted address.
    // Let's just check stability for now.
  }

  // Test flonum equal_hash
  if (equal_hash(make_flonum(123.456), INT32_MAX) != equal_hash(make_flonum(123.456), INT32_MAX)) {
    printf("\033[31m###### equal_hash failed: flonum stability\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test cyclic list (should not hang)
  scm_obj_t head = make_cons(make_fixnum(1), scm_nil);
  scm_cons_rec_t* cons = (scm_cons_rec_t*)head;
  cons->cdr = head;             // create cycle: (1 . #0)
  equal_hash(head, INT32_MAX);  // Should not crash/hang

  // Test cyclic vector (should not hang)
  scm_obj_t vec = make_vector(1, scm_nil);
  scm_obj_t* elts = vector_elts(vec);
  elts[0] = vec;               // create cycle: #(#0)
  equal_hash(vec, INT32_MAX);  // Should not crash/hang

  // Test deep list (should not hang)
  scm_obj_t deep = scm_nil;
  for (int i = 0; i < 1000; i++) {
    deep = make_cons(make_fixnum(i), deep);
  }
  equal_hash(deep, INT32_MAX);  // Should not crash/hang

  printf("\033[32mregression passed\033[0m\n");
  return true;
}

int main(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();
  heap->m_collect_trip_bytes = 1024 * 512;

  test_hashtable();
  test_equal_hash();
  test_regression();

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}
