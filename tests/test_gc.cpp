#include "../src/core.h"
#include "../src/context.h"
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

inline scm_obj_t car(scm_obj_t x) { return ((scm_cons_rec_t*)x)->car; }
inline scm_obj_t cdr(scm_obj_t x) { return ((scm_cons_rec_t*)x)->cdr; }


static bool some_test_failed = false;


static void* subr_stub_ptr = (void*)0xdeadbeef;

static bool test_gc_allocation(int num_loops) {
  object_heap_t* heap = new object_heap_t();

  heap->init(1024 * 1024, 1024 * 256);
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
  heap->init(1024 * 1024 * 4, 1024 * 256);
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

int main(int argc, char** argv) {
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
