#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "../src/context.h"
#include "../src/core.h"
#include "../src/list.h"
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


static void test_cyclic_object_p(scm_obj_t obj, bool expected, const char* name) {
  bool result = cyclic_object_p(obj);
  if (result != expected) {
    printf("\033[31m###### cyclic_object_p failed for %s: %s != %s\033[0m\n", name, result ? "true" : "false", expected ? "true" : "false");
    some_test_failed = true;
  } else {
    printf("\033[32mcyclic_object_p passed for %s\033[0m\n", name);
  }
}

int main(int argc, char** argv) {
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
