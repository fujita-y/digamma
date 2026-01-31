#include "../src/core.h"
#include "../src/hash.h"
#include "../src/object.h"
#include "../src/object_heap.h"

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
  scm_obj_t ht = make_hash_table(string_hash, string_equiv, capacity);

  // Test basic set/get
  scm_obj_t key1 = make_string("foo");
  scm_obj_t val1 = make_fixnum(100);
  hash_table_set(ht, key1, val1);

  scm_obj_t got1 = hash_table_ref(ht, key1, scm_undef);
  if (got1 != val1) {
    printf("\033[31m###### hashtable failed: basic set/get\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test delete
  hash_table_delete(ht, key1);
  scm_obj_t got3 = hash_table_ref(ht, key1, scm_undef);
  if (got3 != scm_undef) {
    printf("\033[31m###### hashtable failed: delete\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test delete non-existent key
  scm_obj_t key_missing = make_string("non-existent");
  hash_table_delete(ht, key_missing);  // should not crash or affect other items

  // Test default value
  scm_obj_t key2 = make_string("bar");
  scm_obj_t def = make_fixnum(999);
  scm_obj_t got2 = hash_table_ref(ht, key2, def);
  if (got2 != def) {
    printf("\033[31m###### hashtable failed: default value\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Test rehash (add more items than capacity)
  for (int i = 0; i < 20; i++) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    hash_table_set(ht, make_string(buf), make_fixnum(i));
  }

  // Delete some items from the expanded table
  for (int i = 0; i < 20; i += 2) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    hash_table_delete(ht, make_string(buf));
  }

  for (int i = 0; i < 20; i++) {
    char buf[32];
    sprintf(buf, "key-%d", i);
    scm_obj_t val = hash_table_ref(ht, make_string(buf), scm_undef);
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

int main(int argc, char** argv) {
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);
  heap.m_collect_trip_bytes = 1024 * 512;

  test_hashtable();

  heap.destroy();

  return some_test_failed ? 1 : 0;
}
