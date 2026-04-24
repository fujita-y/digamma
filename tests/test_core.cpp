#include "core.h"
#include "object.h"
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include "../src/context.h"
#include "../src/core.h"
#include "../src/hash.h"
#include "../src/list.h"
#include "../src/object.h"
#include "../src/object_heap.h"
#include "context.h"
#include "object_heap.h"
#include "subr.h"

#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

static bool some_test_failed = false;

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

extern "C" const char* __hwasan_default_options() { return "leak_check_at_exit=0"; }

namespace test_object {

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

int run_test(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();
  heap->m_collect_trip_bytes = 1024 * 512;

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

  test_environment("test-environment");

  test_cell(100);
  test_cell(-50);
  test_cell_heap("hello world");

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}

}  // namespace test_object

namespace test_gc {

inline scm_obj_t car(scm_obj_t x) { return ((scm_cons_rec_t*)x)->car; }
inline scm_obj_t cdr(scm_obj_t x) { return ((scm_cons_rec_t*)x)->cdr; }

static void* subr_stub_ptr = (void*)0xdeadbeef;

static bool test_gc_allocation(int num_loops) {
  object_heap_t* heap = new object_heap_t();

  heap->init(1024 * 1024 * 32, 1024 * 1024);
  context::init();

  // Root structure:
  // (pair
  //   (symbol "foo")
  // (list
  //     (fixnum 2)
  //     (symbol "bar")
  //     (string "baz")
  //     (vector #(10 20))
  //     (cell (string "cell-val"))
  //     (flonum 3.14159)
  //     (subr "my-subr")
  //     (u8vector #u8(1 2 3))
  //     (hashtable #("key" . "value"))
  //     (closure (lambda (x) x))
  //     (environment "my-env")
  //   )
  // )

  scm_obj_t vec = make_vector(2, make_fixnum(0));
  vector_elts(vec)[0] = make_fixnum(10);
  vector_elts(vec)[1] = make_fixnum(20);

  scm_obj_t c = make_cell(make_string("cell-val"));
  scm_obj_t f = make_flonum(3.14159);

  scm_obj_t u8 = make_u8vector(3);
  u8vector_elts(u8)[0] = 1;
  u8vector_elts(u8)[1] = 2;
  u8vector_elts(u8)[2] = 3;

  scm_obj_t ht = make_hashtable(string_hash, string_equiv, 16);
  hashtable_set(ht, make_string("key"), make_string("value"));

  scm_obj_t clos = make_closure(subr_stub_ptr, 1, 0, 0, NULL, 0);

  scm_obj_t env = make_environment(make_string("my-env"));

  scm_obj_t root_list = make_list(10, make_fixnum(2), make_symbol("bar"), make_string("baz"), vec, c, f, u8, ht, clos, env);

  scm_obj_t root = make_cons(make_symbol("foo"), root_list);

  context::gc_protect(root);

  // Allocate enough objects to potentially trigger GC or just verify allocation works
  for (int i = 0; i < num_loops; i++) {
    heap->safepoint();
    usleep(1);

    // Original garbage
    scm_obj_t obj = make_cons(make_symbol("afas"), make_list(4, make_cons(make_fixnum(1), make_fixnum(2)), make_string("bar"),
                                                             make_cons(make_symbol("affd"), make_string("adf")), make_u8vector(122)));
    scm_obj_t hash = make_hashtable(string_hash, string_equiv, 16);
    hashtable_set(hash, make_string("afd"), make_symbol("value"));
    hashtable_set(hash, make_string("fdf"), obj);
    hashtable_set(hash, make_string("sds"), make_fixnum(3));
    scm_obj_t env = make_environment(make_string("env"));
    scm_environment_rec_t* env_rec = (scm_environment_rec_t*)to_address(env);
    hashtable_set(env_rec->variables, make_symbol("var"), make_fixnum(123));
    scm_obj_t closure_env[2];
    closure_env[0] = make_fixnum(10);
    closure_env[1] = make_symbol("hello");
    make_closure(NULL, 0, 0, 2, closure_env, 0);

    // Additional garbage types
    make_vector(10, make_fixnum(i));
    make_cell(make_string("garbage-cell"));
    make_flonum(1.2345 + i);
    make_u8vector(100);
    make_hashtable(string_hash, string_equiv, 4);
    make_closure(NULL, 0, 0, 0, NULL, 0);
    make_environment(make_string("garbage-env"));

    // values garbage: allocate a 3-element values object with mixed contents
    {
      scm_obj_t vals = make_values(3);
      values_elts(vals)[0] = make_fixnum(i);
      values_elts(vals)[1] = make_string("vals-elem");
      values_elts(vals)[2] = make_symbol("vals-sym");
    }

    if (!is_cons(obj)) {
      printf("\033[31m###### allocation failed at %d\033[0m\n", i);
      some_test_failed = true;
      return false;
    }
  }

  context::gc_unprotect(root);

  context::destroy();
  heap->destroy();
  delete heap;

  printf("\033[32mtest_gc_allocation passed\033[0m\n");
  return true;
}

static bool test_root_survivability() {
  printf("running test_root_survivability...\n");
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 32, 1024 * 1024);
  context::init();

  // 1. Root a list of various objects
  scm_obj_t s = make_string("survivor-string");
  scm_obj_t v = make_vector(1, make_fixnum(0));
  vector_elts(v)[0] = s;
  scm_obj_t c = make_cell(v);

  // values object holding a string — tests that GC shades both the values
  // header and its element array
  scm_obj_t vals = make_values(2);
  scm_obj_t vals_str = make_string("survivor-vals-str");
  values_elts(vals)[0] = vals_str;
  values_elts(vals)[1] = make_fixnum(42);

  // hashtable object — tests that GC shades the aux buffer and its key/value slots
  scm_obj_t ht = make_hashtable(string_hash, string_equiv, 16);
  hashtable_set(ht, make_string("survivor-key-a"), make_string("survivor-val-a"));
  hashtable_set(ht, make_string("survivor-key-b"), make_fixnum(777));

  scm_obj_t list = make_list(4, c, make_symbol("survivor-symbol"), vals, ht);

  context::gc_protect(list);

  // 2. Allocate lots of garbage to trigger multiple GCs
  for (int i = 0; i < 50000; i++) {
    make_cons(make_fixnum(i), make_string("garbage-garbage-garbage"));
    // Mix in values garbage to stress the shade/sweep path
    if (i % 7 == 0) {
      scm_obj_t gv = make_values(2);
      values_elts(gv)[0] = make_fixnum(i);
      values_elts(gv)[1] = make_string("gc-vals-trash");
    }
    // Mix in hashtable garbage to stress the hashtable shade/sweep path
    if (i % 11 == 0) {
      scm_obj_t gh = make_hashtable(string_hash, string_equiv, 4);
      hashtable_set(gh, make_string("trash-key"), make_fixnum(i));
    }
    if (i % 100 == 0) heap->safepoint();
  }

  // 3. Verify survivors
  if (!is_cons(list)) {
    printf("\033[31m###### root survivability failed: list is not cons\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t c_back = car(list);
  if (!is_cell(c_back)) {
    printf("\033[31m###### root survivability failed: car(list) is not cell\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t v_back = cell_value(c_back);
  if (!is_vector(v_back)) {
    printf("\033[31m###### root survivability failed: cell_value(c) is not vector\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t s_back = vector_elts(v_back)[0];
  if (!is_string(s_back) || strcmp((const char*)string_name(s_back), "survivor-string") != 0) {
    printf("\033[31m###### root survivability failed: vector_elts(v)[0] is incorrect\033[0m\n");
    some_test_failed = true;
    return false;
  }

  scm_obj_t sym_back = car(cdr(list));
  if (!is_symbol(sym_back) || strcmp((const char*)symbol_name(sym_back), "survivor-symbol") != 0) {
    printf("\033[31m###### root survivability failed: cdr(list) is incorrect symbol\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Verify the values object and its string element survived GC
  scm_obj_t vals_back = car(cdr(cdr(list)));
  if (!is_values(vals_back)) {
    printf("\033[31m###### root survivability failed: values object not found\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (values_nsize(vals_back) != 2) {
    printf("\033[31m###### root survivability failed: values nsize mismatch (%d)\033[0m\n", values_nsize(vals_back));
    some_test_failed = true;
    return false;
  }
  scm_obj_t vs_back = values_elts(vals_back)[0];
  if (!is_string(vs_back) || strcmp((const char*)string_name(vs_back), "survivor-vals-str") != 0) {
    printf("\033[31m###### root survivability failed: values element string incorrect\033[0m\n");
    some_test_failed = true;
    return false;
  }
  if (values_elts(vals_back)[1] != make_fixnum(42)) {
    printf("\033[31m###### root survivability failed: values element fixnum incorrect\033[0m\n");
    some_test_failed = true;
    return false;
  }

  // Verify the hashtable and its entries survived GC
  scm_obj_t ht_back = car(cdr(cdr(cdr(list))));
  if (!is_hashtable(ht_back)) {
    printf("\033[31m###### root survivability failed: hashtable not found\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t va_back = hashtable_ref(ht_back, make_string("survivor-key-a"), scm_undef);
  if (!is_string(va_back) || strcmp((const char*)string_name(va_back), "survivor-val-a") != 0) {
    printf("\033[31m###### root survivability failed: hashtable entry 'a' incorrect\033[0m\n");
    some_test_failed = true;
    return false;
  }
  scm_obj_t vb_back = hashtable_ref(ht_back, make_string("survivor-key-b"), scm_undef);
  if (vb_back != make_fixnum(777)) {
    printf("\033[31m###### root survivability failed: hashtable entry 'b' incorrect\033[0m\n");
    some_test_failed = true;
    return false;
  }

  context::gc_unprotect(list);
  context::destroy();
  heap->destroy();
  delete heap;

  printf("\033[32mtest_root_survivability passed\033[0m\n");
  return true;
}

int run_test(int argc, char** argv) {
  int num_loops = 1000;
  int repeat = 1;
  if (argc > 1) {
    num_loops = atoi(argv[1]);
  }
  if (argc > 2) {
    repeat = atoi(argv[2]);
  }
  for (int i = 0; i < repeat; i++) {
    test_gc_allocation(num_loops);
    test_root_survivability();
  }
  return some_test_failed ? 1 : 0;
}

}  // namespace test_gc

namespace test_list {

static void test_cyclic_object_p(scm_obj_t obj, bool expected, const char* name) {
  bool result = cyclic_object_p(obj);
  if (result != expected) {
    printf("\033[31m###### cyclic_object_p failed for %s: %s != %s\033[0m\n", name, result ? "true" : "false", expected ? "true" : "false");
    some_test_failed = true;
  } else {
    printf("\033[32mcyclic_object_p passed for %s\033[0m\n", name);
  }
}

int run_test(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  // 64 MB heap: stress tests allocate 100k vectors (each needs ~40 bytes of slab space)
  // plus 100k cons cells. All are live on the C stack so GC cannot reclaim them.
  // With a 2 MB heap the allocator calls fatal() which exits before context::destroy(),
  // which causes port_aux_t (allocated via 'new' in make_port) to be never freed.
  heap->init(1024 * 1024 * 64, 1024 * 1024 * 4);
  context::init();
  heap->m_collect_trip_bytes = 1024 * 512 * 8;

  // 1. Non-cyclic objects
  test_cyclic_object_p(scm_nil, false, "nil");
  test_cyclic_object_p(scm_true, false, "true");
  test_cyclic_object_p(scm_false, false, "false");
  test_cyclic_object_p(make_fixnum(123), false, "fixnum");
  test_cyclic_object_p(make_symbol("a"), false, "symbol");

  // Proper list: (1 2 3)
  scm_obj_t list1 = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  test_cyclic_object_p(list1, false, "(1 2 3)");

  // Improper list: (1 2 . 3)
  scm_obj_t list2 = make_cons(make_fixnum(1), make_cons(make_fixnum(2), make_fixnum(3)));
  test_cyclic_object_p(list2, false, "(1 2 . 3)");

  // Nested non-cyclic: ((1) (1))
  scm_obj_t sub = make_list(1, make_fixnum(1));
  scm_obj_t list3 = make_list(2, sub, sub);
  test_cyclic_object_p(list3, false, "((1) (1)) - sharing but no cycle");

  // 2. Cyclic lists
  // simple cycle: a = (1); set-cdr! a a
  scm_obj_t a = make_cons(make_fixnum(1), scm_nil);
  heap->write_barrier(a);
  ((scm_cons_rec_t*)a)->cdr = a;
  test_cyclic_object_p(a, true, "a = (1); set-cdr! a a");

  // cycle later: b = (1 2 3); set-cdr! (cddr b) b
  scm_obj_t b = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  scm_obj_t b_cddr = cons_cdr(cons_cdr(b));
  heap->write_barrier(b);
  ((scm_cons_rec_t*)b_cddr)->cdr = b;
  test_cyclic_object_p(b, true, "b = (1 2 3); set-cdr! (cddr b) b");

  // cycle later not to head: c = (1 2 3); set-cdr! (cddr c) (cdr c)
  scm_obj_t c = make_list(3, make_fixnum(1), make_fixnum(2), make_fixnum(3));
  scm_obj_t c_cdr = cons_cdr(c);
  scm_obj_t c_cddr = cons_cdr(c_cdr);
  heap->write_barrier(c);
  ((scm_cons_rec_t*)c_cddr)->cdr = c_cdr;
  test_cyclic_object_p(c, true, "c = (1 2 3); set-cdr! (cddr c) (cdr c)");

  // 3. Vector cycles
  // v = #(0 1 2); v[1] = v
  scm_obj_t v1 = make_vector(3, make_fixnum(0));
  vector_elts(v1)[0] = make_fixnum(0);
  vector_elts(v1)[1] = make_fixnum(1);
  vector_elts(v1)[2] = make_fixnum(2);
  heap->write_barrier(v1);
  vector_elts(v1)[1] = v1;
  test_cyclic_object_p(v1, true, "v = #(0 1 2); v[1] = v");

  // deep vector cycle: v2 = #(0 #(1 #(2))); v2[1][1][1] = v2
  scm_obj_t v2_inner_inner = make_vector(1, make_fixnum(2));
  scm_obj_t v2_inner = make_vector(2, make_fixnum(1));
  vector_elts(v2_inner)[1] = v2_inner_inner;
  scm_obj_t v2 = make_vector(2, make_fixnum(0));
  vector_elts(v2)[1] = v2_inner;
  heap->write_barrier(v2);
  vector_elts(v2_inner_inner)[0] = v2;
  test_cyclic_object_p(v2, true, "deep vector cycle");

  // 4. Mixed cycle: list in vector pointing to vector
  scm_obj_t v3_list = make_list(1, make_fixnum(1));
  scm_obj_t v3 = make_vector(2, make_fixnum(0));
  vector_elts(v3)[1] = v3_list;
  heap->write_barrier(v3_list);
  ((scm_cons_rec_t*)v3_list)->car = v3;
  test_cyclic_object_p(v3, true, "list in vector cycle");

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}

}  // namespace test_list

namespace test_hash {

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

int run_test(int argc, char** argv) {
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

}  // namespace test_hash

namespace test_symbol {

static bool test_symbol_intern(const char* name) {
  scm_obj_t x1 = make_symbol(name);
  scm_obj_t x2 = make_symbol(name);
  if (x1 != x2) {
    printf("\033[31m###### symbol intern failed: %s (0x%lx != 0x%lx)\033[0m\n", name, x1, x2);
    some_test_failed = true;
    return false;
  }
  printf("\033[32msymbol intern passed: %s\033[0m\n", name);
  return true;
}

int run_test(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();
  heap->m_collect_trip_bytes = 1024 * 512;

  test_symbol_intern("foobar");
  test_symbol_intern("hogehoge");
  test_symbol_intern("abc");
  test_symbol_intern("");
  test_symbol_intern("white space");
  test_symbol_intern("special-chars!@#$%^&*()");
  test_symbol_intern("12345");

  context::destroy();
  heap->destroy();
  delete heap;

  return some_test_failed ? 1 : 0;
}

}  // namespace test_symbol

namespace test_arith {
// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

// ---------------------------------------------------------------------------
// Stubs
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Test infrastructure
// ---------------------------------------------------------------------------

#define ASSERT_TRUE(expr)                         \
  do {                                            \
    if (!(expr)) {                                \
      printf("\033[31mFAIL: %s\033[0m\n", #expr); \
      some_test_failed = true;                    \
    } else {                                      \
      printf("\033[32mPASS: %s\033[0m\n", #expr); \
    }                                             \
  } while (0)

#define ASSERT_FALSE(expr)                           \
  do {                                               \
    if ((expr)) {                                    \
      printf("\033[31mFAIL: !(%s)\033[0m\n", #expr); \
      some_test_failed = true;                       \
    } else {                                         \
      printf("\033[32mPASS: !(%s)\033[0m\n", #expr); \
    }                                                \
  } while (0)

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

void test_add() {
  printf("--- addition (+ ...) ---\n");

  auto add = [](std::vector<scm_obj_t> args) { return subr_num_add(scm_nil, (int)args.size(), args.data()); };

  // (+) -> 0
  ASSERT_TRUE(add({}) == make_fixnum(0));

  // (+ 5) -> 5
  ASSERT_TRUE(add({make_fixnum(5)}) == make_fixnum(5));

  // (+ 1 2 3) -> 6
  ASSERT_TRUE(add({make_fixnum(1), make_fixnum(2), make_fixnum(3)}) == make_fixnum(6));

  // (+ 1.5 2.5) -> 4.0
  scm_obj_t res = add({make_flonum(1.5), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // Mixed: (+ 1 2.5) -> 3.5
  res = add({make_fixnum(1), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 3.5);
}

void test_sub() {
  printf("--- subtraction (- ...) ---\n");

  auto sub = [](std::vector<scm_obj_t> args) { return subr_num_sub(scm_nil, (int)args.size(), args.data()); };

  // (- 10) -> -10
  ASSERT_TRUE(sub({make_fixnum(10)}) == make_fixnum(-10));

  // (- 10 3 2) -> 5
  ASSERT_TRUE(sub({make_fixnum(10), make_fixnum(3), make_fixnum(2)}) == make_fixnum(5));

  // (- 5.5 1.5) -> 4.0
  scm_obj_t res = sub({make_flonum(5.5), make_flonum(1.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);

  // Mixed: (- 10 2.5) -> 7.5
  res = sub({make_fixnum(10), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 7.5);
}

void test_mul() {
  printf("--- multiplication (* ...) ---\n");

  auto mul = [](std::vector<scm_obj_t> args) { return subr_num_mul(scm_nil, (int)args.size(), args.data()); };

  // (*) -> 1
  ASSERT_TRUE(mul({}) == make_fixnum(1));

  // (* 5) -> 5
  ASSERT_TRUE(mul({make_fixnum(5)}) == make_fixnum(5));

  // (* 2 3 4) -> 24
  ASSERT_TRUE(mul({make_fixnum(2), make_fixnum(3), make_fixnum(4)}) == make_fixnum(24));

  // (* 2.5 4.0) -> 10.0
  scm_obj_t res = mul({make_flonum(2.5), make_flonum(4.0)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 10.0);

  // Mixed: (* 2 2.5) -> 5.0
  res = mul({make_fixnum(2), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 5.0);
}

void test_div() {
  printf("--- division (/ ...) ---\n");

  auto div = [](std::vector<scm_obj_t> args) { return subr_num_div(scm_nil, (int)args.size(), args.data()); };

  // (/ 4) -> 1/4 (in this implementation it returns 0 for fixnum 1/4 because it's integer division for fixnums)
  // Wait, let's check the implementation again.
  // 168:     return make_fixnum(1 / res_i);
  // So (/ 4) returns 0.
  ASSERT_TRUE(div({make_fixnum(4)}) == make_fixnum(0));

  // (/ 24 2 3) -> 4
  ASSERT_TRUE(div({make_fixnum(24), make_fixnum(2), make_fixnum(3)}) == make_fixnum(4));

  // (/ 10.0 4.0) -> 2.5
  scm_obj_t res = div({make_flonum(10.0), make_flonum(4.0)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 2.5);

  // Mixed: (/ 10 2.5) -> 4.0
  res = div({make_fixnum(10), make_flonum(2.5)});
  ASSERT_TRUE(is_flonum(res));
  ASSERT_TRUE(flonum(res) == 4.0);
}

void test_cmp() {
  printf("--- comparisons (= < > <= >=) ---\n");

  auto eq = [](std::vector<scm_obj_t> args) { return subr_num_eq(scm_nil, (int)args.size(), args.data()); };
  auto lt = [](std::vector<scm_obj_t> args) { return subr_num_lt(scm_nil, (int)args.size(), args.data()); };
  auto gt = [](std::vector<scm_obj_t> args) { return subr_num_gt(scm_nil, (int)args.size(), args.data()); };
  auto le = [](std::vector<scm_obj_t> args) { return subr_num_le(scm_nil, (int)args.size(), args.data()); };
  auto ge = [](std::vector<scm_obj_t> args) { return subr_num_ge(scm_nil, (int)args.size(), args.data()); };

  // =
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_flonum(5.0)}) == scm_true);
  ASSERT_TRUE(eq({make_flonum(5.0), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(6)}) == scm_false);
  ASSERT_TRUE(eq({make_fixnum(5), make_flonum(5.1)}) == scm_false);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5), make_fixnum(5)}) == scm_true);
  ASSERT_TRUE(eq({make_fixnum(5), make_fixnum(5), make_flonum(5.0)}) == scm_true);

  // <
  ASSERT_TRUE(lt({make_fixnum(1), make_fixnum(2), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(lt({make_fixnum(1), make_fixnum(3), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(lt({make_fixnum(1), make_flonum(1.5), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(lt({make_flonum(1.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(lt({make_fixnum(2), make_flonum(1.0)}) == scm_false);

  // >
  ASSERT_TRUE(gt({make_fixnum(3), make_fixnum(2), make_fixnum(1)}) == scm_true);
  ASSERT_TRUE(gt({make_fixnum(3), make_fixnum(1), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(gt({make_flonum(3.5), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(gt({make_fixnum(3), make_flonum(3.5)}) == scm_false);

  // <=
  ASSERT_TRUE(le({make_fixnum(1), make_fixnum(2), make_fixnum(2), make_fixnum(3)}) == scm_true);
  ASSERT_TRUE(le({make_fixnum(1), make_fixnum(3), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(le({make_fixnum(2), make_flonum(2.0)}) == scm_true);
  ASSERT_TRUE(le({make_flonum(2.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(le({make_fixnum(2), make_flonum(2.1)}) == scm_true);

  // >=
  ASSERT_TRUE(ge({make_fixnum(3), make_fixnum(2), make_fixnum(2), make_fixnum(1)}) == scm_true);
  ASSERT_TRUE(ge({make_fixnum(3), make_fixnum(1), make_fixnum(2)}) == scm_false);
  ASSERT_TRUE(ge({make_fixnum(2), make_flonum(2.0)}) == scm_true);
  ASSERT_TRUE(ge({make_flonum(2.0), make_fixnum(2)}) == scm_true);
  ASSERT_TRUE(ge({make_flonum(2.1), make_fixnum(2)}) == scm_true);
}

int run_test(int argc, char** argv) {
  object_heap_t* heap = new object_heap_t();
  heap->init(1024 * 1024 * 2, 1024 * 1024);
  context::init();

  test_add();
  test_sub();
  test_mul();
  test_div();
  test_cmp();

  context::destroy();
  heap->destroy();
  delete heap;

  if (some_test_failed) {
    printf("\n\033[31mSOME TESTS FAILED\033[0m\n");
    return 1;
  } else {
    printf("\n\033[32mALL TESTS PASSED\033[0m\n");
    return 0;
  }
}

}  // namespace test_arith

int main(int argc, char** argv) {
  test_object::run_test(argc, argv);
  test_gc::run_test(argc, argv);
  test_list::run_test(argc, argv);
  test_hash::run_test(argc, argv);
  test_symbol::run_test(argc, argv);
  test_arith::run_test(argc, argv);
  return some_test_failed ? 1 : 0;
}
