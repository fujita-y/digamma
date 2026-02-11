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

inline scm_obj_t car(scm_obj_t x) { return ((scm_cons_rec_t*)x)->car; }
inline scm_obj_t cdr(scm_obj_t x) { return ((scm_cons_rec_t*)x)->cdr; }

static bool some_test_failed = false;

static void* subr_stub_ptr = (void*)0xdeadbeef;

static bool test_gc_allocation(int num_loops) {
  object_heap_t heap;

  heap.init(1024 * 1024, 1024 * 256);

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
  scm_obj_t s = make_subr(make_symbol("my-subr"), 1, 0, subr_stub_ptr);

  scm_obj_t u8 = make_u8vector(3);
  u8vector_elts(u8)[0] = 1;
  u8vector_elts(u8)[1] = 2;
  u8vector_elts(u8)[2] = 3;

  scm_obj_t ht = make_hashtable(string_hash, string_equiv, 16);
  hashtable_set(ht, make_string("key"), make_string("value"));

  scm_obj_t clos = make_closure(subr_stub_ptr, 1, 0, 0, NULL, scm_unspecified);

  scm_obj_t env = make_environment(make_string("my-env"));

  scm_obj_t root_list = make_list(11, make_fixnum(2), make_symbol("bar"), make_string("baz"), vec, c, f, s, u8, ht, clos, env);

  scm_obj_t root = make_cons(make_symbol("foo"), root_list);

  heap.add_root(root);

  // Allocate enough objects to potentially trigger GC or just verify allocation works
  for (int i = 0; i < num_loops; i++) {
    heap.safepoint();
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
    make_closure(NULL, 0, 0, 2, closure_env, scm_true);

    // Additional garbage types
    make_vector(10, make_fixnum(i));
    make_cell(make_string("garbage-cell"));
    make_flonum(1.2345 + i);
    make_subr(make_symbol("garbage-subr"), 0, 0, NULL);
    make_u8vector(100);
    make_hashtable(string_hash, string_equiv, 4);
    make_closure(NULL, 0, 0, 0, NULL, scm_false);
    make_environment(make_string("garbage-env"));

    if (!is_cons(obj)) {
      printf("\033[31m###### allocation failed at %d\033[0m\n", i);
      some_test_failed = true;
      return false;
    }
  }

  // Verify root object is preserved
  if (!is_cons(root)) {
    printf("\033[31mroot object corrupted (not cons)\033[0m\n");
    return false;
  }
  if (!is_symbol(car(root))) {
    printf("\033[31mroot object corrupted (car not symbol)\033[0m\n");
    return false;
  }

  scm_obj_t sym_name = car(root);
  std::string s_name = std::string((char*)symbol_name(sym_name));
  if (s_name != "foo") {
    printf("\033[31mroot object corrupted (car is %s, expected foo)\033[0m\n", s_name.c_str());
    return false;
  }

  scm_obj_t list = cdr(root);

  // 1. fixnum 2
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (cdr not list)\033[0m\n");
    return false;
  }
  if (!is_fixnum(car(list)) || fixnum(car(list)) != 2) {
    printf("\033[31mroot object corrupted (1st elt)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 2. symbol "bar"
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (2nd cons)\033[0m\n");
    return false;
  }
  if (!is_symbol(car(list))) {
    printf("\033[31mroot object corrupted (2nd elt type)\033[0m\n");
    return false;
  }
  if (std::string((char*)symbol_name(car(list))) != "bar") {
    printf("\033[31mroot object corrupted (2nd elt value)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 3. string "baz"
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (3rd cons)\033[0m\n");
    return false;
  }
  if (!is_string(car(list))) {
    printf("\033[31mroot object corrupted (3rd elt type)\033[0m\n");
    return false;
  }
  if (std::string((char*)string_name(car(list))) != "baz") {
    printf("\033[31mroot object corrupted (3rd elt value)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 4. vector #(10 20)
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (4th cons)\033[0m\n");
    return false;
  }
  if (!is_vector(car(list))) {
    printf("\033[31mroot object corrupted (4th elt type)\033[0m\n");
    return false;
  }
  scm_obj_t v_obj = car(list);
  if (vector_nsize(v_obj) != 2) {
    printf("\033[31mroot object corrupted (vector size)\033[0m\n");
    return false;
  }
  if (fixnum(vector_elts(v_obj)[0]) != 10) {
    printf("\033[31mroot object corrupted (vector elt 0)\033[0m\n");
    return false;
  }
  if (fixnum(vector_elts(v_obj)[1]) != 20) {
    printf("\033[31mroot object corrupted (vector elt 1)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 5. cell (string "cell-val")
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (5th cons)\033[0m\n");
    return false;
  }
  if (!is_cell(car(list))) {
    printf("\033[31mroot object corrupted (5th elt type)\033[0m\n");
    return false;
  }
  scm_obj_t c_obj = car(list);
  scm_obj_t c_val = cell_value(c_obj);
  if (!is_string(c_val)) {
    printf("\033[31mroot object corrupted (cell val type)\033[0m\n");
    return false;
  }
  if (std::string((char*)string_name(c_val)) != "cell-val") {
    printf("\033[31mroot object corrupted (cell val content)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 6. flonum 3.14159
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (6th cons)\033[0m\n");
    return false;
  }
  if (!is_tc6(car(list), tc6_long_flonum) && !is_short_flonum(car(list))) {  // Check generic flonum
    printf("\033[31mroot object corrupted (6th elt type)\033[0m\n");
    return false;
  }
  double f_val = flonum(car(list));
  if (abs(f_val - 3.14159) > 1e-6) {
    printf("\033[31mroot object corrupted (flonum val)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 7. subr "my-subr"
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (7th cons)\033[0m\n");
    return false;
  }
  if (!is_subr(car(list))) {
    printf("\033[31mroot object corrupted (7th elt type)\033[0m\n");
    return false;
  }
  scm_subr_rec_t* subr_rec = (scm_subr_rec_t*)to_address(car(list));
  if (std::string((char*)symbol_name(subr_rec->name)) != "my-subr") {
    printf("\033[31mroot object corrupted (subr name)\033[0m\n");
    return false;
  }
  if (subr_rec->code != subr_stub_ptr) {
    printf("\033[31mroot object corrupted (subr code)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 8. u8vector #u8(1 2 3)
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (8th cons)\033[0m\n");
    return false;
  }
  if (!is_u8vector(car(list))) {
    printf("\033[31mroot object corrupted (8th elt type)\033[0m\n");
    return false;
  }
  scm_obj_t u8_obj = car(list);
  if (u8vector_nsize(u8_obj) != 3) {
    printf("\033[31mroot object corrupted (u8vector size)\033[0m\n");
    return false;
  }
  if (u8vector_elts(u8_obj)[0] != 1) {
    printf("\033[31mroot object corrupted (u8vector elt 0)\033[0m\n");
    return false;
  }
  if (u8vector_elts(u8_obj)[1] != 2) {
    printf("\033[31mroot object corrupted (u8vector elt 1)\033[0m\n");
    return false;
  }
  if (u8vector_elts(u8_obj)[2] != 3) {
    printf("\033[31mroot object corrupted (u8vector elt 2)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 9. hashtable #("key" . "value")
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (9th cons)\033[0m\n");
    return false;
  }
  if (!is_hashtable(car(list))) {
    printf("\033[31mroot object corrupted (9th elt type)\033[0m\n");
    return false;
  }
  // Detailed hashtable check omitted for brevity, but at least type is checked

  list = cdr(list);

  // 10. closure
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (10th cons)\033[0m\n");
    return false;
  }
  if (!is_closure(car(list))) {
    printf("\033[31mroot object corrupted (10th elt type)\033[0m\n");
    return false;
  }

  list = cdr(list);

  // 11. environment "my-env"
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (11th cons)\033[0m\n");
    return false;
  }
  if (!is_environment(car(list))) {
    printf("\033[31mroot object corrupted (11th elt type)\033[0m\n");
    return false;
  }
  scm_environment_rec_t* env_rec2 = (scm_environment_rec_t*)to_address(car(list));
  if (std::string((char*)string_name(env_rec2->name)) != "my-env") {
    printf("\033[31mroot object corrupted (env name)\033[0m\n");
    return false;
  }

  if (cdr(list) != scm_nil) {
    printf("\033[31mroot object corrupted (list not terminated)\033[0m\n");
    return false;
  }

  heap.remove_root(root);

  heap.destroy();

  printf("\033[32mtest_gc_allocation passed\033[0m\n");
  return true;
}

int main(int argc, char** argv) {
  int num_loops = 10000;
  int repeat = 1;
  if (argc > 1) {
    num_loops = atoi(argv[1]);
  }
  if (argc > 2) {
    repeat = atoi(argv[2]);
  }
  for (int i = 0; i < repeat; i++) {
    test_gc_allocation(num_loops);
  }
  return some_test_failed ? 1 : 0;
}
