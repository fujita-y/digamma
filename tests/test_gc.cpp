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

static bool test_gc_allocation(int num_loops) {
  object_heap_t heap;

  heap.init(1024 * 256, 1024 * 64);

  scm_obj_t root = make_cons(make_symbol("foo"), make_list(3, make_fixnum(2), make_symbol("bar"), make_string("baz")));
  heap.add_root(root);

  // Allocate enough objects to potentially trigger GC or just verify allocation works
  for (int i = 0; i < num_loops; i++) {
    heap.safepoint();
    usleep(1);
    scm_obj_t obj = make_cons(make_symbol("afas"), make_list(4, make_cons(make_fixnum(1), make_fixnum(2)), make_string("bar"),
                                                             make_cons(make_symbol("affd"), make_string("adf")), make_u8vector(122)));
    scm_obj_t hash = make_hashtable(string_hash, string_equiv, 16);
    hashtable_set(hash, make_string("afd"), make_symbol("value"));
    hashtable_set(hash, make_string("fdf"), obj);
    hashtable_set(hash, make_string("sds"), make_fixnum(3));
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
  if (!is_cons(list)) {
    printf("\033[31mroot object corrupted (cdr not list)\033[0m\n");
    return false;
  }
  if (!is_fixnum(car(list)) || fixnum(car(list)) != 2) {
    printf("\033[31mroot object corrupted (1st elt)\033[0m\n");
    return false;
  }

  list = cdr(list);
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
  if (argc > 1) {
    num_loops = atoi(argv[1]);
  }
  test_gc_allocation(num_loops);
  return some_test_failed ? 1 : 0;
}
