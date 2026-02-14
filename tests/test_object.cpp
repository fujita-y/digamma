#include <cstring>
#include "../src/core.h"
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

static bool test_fixnum_in_range(int64_t i64) {
  int64_t x1 = i64;
  scm_obj_t x2 = make_fixnum(x1);
  int64_t x3 = fixnum(x2);
  if (x1 != x3) {
    printf("\033[31m###### fixnum failed: %ld != %ld\033[0m\n", x1, x3);
    some_test_failed = true;
    return false;
  }
  printf("\033[32mfixnum passed: %ld\033[0m\n", x1);
  return true;
}

static bool test_flonum(double d) {
  double x4 = d;
  scm_obj_t x5 = make_flonum(x4);
  double x6 = flonum(x5);
  if (x4 != x6) {
    printf("\033[31m###### flonum failed: %.17e != %.17e\033[0m\n", x4, x6);
    some_test_failed = true;
    return false;
  }
  printf("\033[32mflonum passed: %.17e (%s)\033[0m\n", x4, is_short_flonum(x5) ? "short" : "long");
  return true;
}

static bool test_symbol(const char* name) {
  scm_obj_t x1 = make_symbol(name);
  uint8_t* x2 = symbol_name(x1);
  if (strcmp(name, (const char*)x2) != 0) {
    printf("\033[31m###### symbol failed: %s != %s\033[0m\n", name, (const char*)x2);
    some_test_failed = true;
    return false;
  }
  printf("\033[32msymbol passed: %s\033[0m\n", name);
  return true;
}

static bool test_string(const char* name) {
  scm_obj_t x1 = make_string(name);
  uint8_t* x2 = string_name(x1);
  if (strcmp(name, (const char*)x2) != 0) {
    printf("\033[31m###### string failed: %s != %s\033[0m\n", name, (const char*)x2);
    some_test_failed = true;
    return false;
  }
  printf("\033[32msymbol passed: %s\033[0m\n", name);
  return true;
}

static bool test_vector(int nsize) {
  scm_obj_t x1 = make_vector(nsize, make_fixnum(nsize));
  int n = vector_nsize(x1);
  if (n != nsize) {
    printf("\033[31m###### vector count failed: %d != %d\033[0m\n", nsize, n);
    some_test_failed = true;
    return false;
  }
  scm_obj_t* elts = vector_elts(x1);
  while (n-- > 0) {
    if (elts[n] != make_fixnum(nsize)) {
      printf("\033[31m###### vector elts failed: %d != %ld\033[0m\n", nsize, fixnum(elts[n]));
      some_test_failed = true;
      return false;
    }
  }
  printf("\033[32mvector passed: %d\033[0m\n", nsize);
  return true;
}

static bool test_u8vector(int nsize) {
  scm_obj_t x1 = make_u8vector(nsize);
  int n = u8vector_nsize(x1);
  if (n != nsize) {
    printf("\033[31m###### u8vector count failed: %d != %d\033[0m\n", nsize, n);
    some_test_failed = true;
    return false;
  }
  uint8_t* elts = u8vector_elts(x1);
  while (n-- > 0) {
    if (elts[n] != 0) {
      printf("\033[31m###### u8vector elts failed: %d != %d\033[0m\n", 0, elts[n]);
      some_test_failed = true;
      return false;
    }
  }
  printf("\033[32mu8vector passed: %d\033[0m\n", nsize);
  return true;
}

static bool test_environment(const char* name) {
  scm_obj_t x1 = make_symbol(name);
  scm_obj_t x2 = make_environment(x1);
  if (!is_environment(x2)) {
    printf("\033[31m###### environment failed: not an environment\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_environment_rec_t* rec = (scm_environment_rec_t*)to_address(x2);
  if (rec->name != x1) {
    printf("\033[31m###### environment failed: name mismatch\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (!is_hashtable(rec->variables)) {
    printf("\033[31m###### environment failed: variable is not hashtable\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_hashtable_rec_t* var_ht = (scm_hashtable_rec_t*)to_address(rec->variables);
  if (var_ht->aux->used != 0) {
    printf("\033[31m###### environment failed: variable hashtable not empty\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (!is_hashtable(rec->macros)) {
    printf("\033[31m###### environment failed: macro is not hashtable\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_hashtable_rec_t* mac_ht = (scm_hashtable_rec_t*)to_address(rec->macros);
  if (mac_ht->aux->used != 0) {
    printf("\033[31m###### environment failed: macro hashtable not empty\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32menvironment passed: %s\033[0m\n", name);
  return true;
}

static bool test_cell_heap(const char* val) {
  scm_obj_t v = make_string(val);
  scm_obj_t c = make_cell(v);
  if (!is_cell(c)) {
    printf("\033[31m###### cell heap failed: is_cell returned false\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (cell_value(c) != v) {
    printf("\033[31m###### cell heap value failed: obj != obj\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t v2 = make_string("foobar");
  cell_value_set(c, v2);
  if (cell_value(c) != v2) {
    printf("\033[31m###### cell heap value update failed: obj != obj\033[0m\n");
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcell heap passed: %s\033[0m\n", val);
  return true;
}

static bool test_cell(int64_t val) {
  scm_obj_t v = make_fixnum(val);
  scm_obj_t c = make_cell(v);
  if (!is_cell(c)) {
    printf("\033[31m###### cell failed: is_cell returned false\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (cell_value(c) != v) {
    printf("\033[31m###### cell value failed: %ld != %ld\033[0m\n", fixnum(cell_value(c)), val);
    some_test_failed = true;
    return false;
  }
  scm_obj_t v2 = make_fixnum(val + 1);
  cell_value_set(c, v2);
  if (cell_value(c) != v2) {
    printf("\033[31m###### cell value update failed: %ld != %ld\033[0m\n", fixnum(cell_value(c)), val + 1);
    some_test_failed = true;
    return false;
  }
  printf("\033[32mcell passed: %ld\033[0m\n", val);
  return true;
}

int main(int argc, char** argv) {
  object_heap_t heap;
  heap.init(1024 * 1024 * 2, 1024 * 1024);
  heap.m_collect_trip_bytes = 1024 * 512;

  test_fixnum_in_range(-1);
  test_fixnum_in_range(0);
  test_fixnum_in_range(1);

  test_flonum(0.0);
  test_flonum(-0.0);
  test_flonum(1.3e20);
  test_flonum(-1.3e20);
  test_flonum(5.87747175411143884e-39);  // 0x3800000000000001 short
  test_flonum(6.8056473384187685e+38);   // 0x47ffffffffffffff short

  test_flonum(5.87747175411143754e-39);  // 0x3800000000000000 long
  test_flonum(5.87747175411143689e-39);  // 0x37ffffffffffffff long
  test_flonum(6.80564733841876927e+38);  // 0x4800000000000000 long
  test_flonum(6.80564733841877985e+38);  // 0x4800000000000007 long
  test_flonum(1.3e-100);
  test_flonum(1.3e+100);

  test_symbol("foobar");
  test_symbol("hogehoge");

  test_string("quick brown fox");
  test_string("jump over lazy dog");

  test_vector(16);
  test_vector(65);

  test_u8vector(97);
  test_u8vector(122);

  test_environment("interaction-environment");

  test_cell(100);
  test_cell(-50);
  test_cell_heap("hello world");

  heap.destroy();

  return some_test_failed ? 1 : 0;
}
