// Simple Scheme Interpreter using Concurrent GC
// Copyright (c) 2026

#include "core.h"
#include <cctype>
#include <cstring>
#include <functional>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
#include "concurrent_heap.h"
#include "concurrent_pool.h"

#include <cstdarg>

using namespace std;

// -- Utils --
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

// -- Globals --
concurrent_heap_t heap;
concurrent_pool_t pool;
concurrent_slab_t slab_16;
concurrent_slab_t slab_32;

// -- Tagging --
// We use low bits for tagging.
// xxx0 : Heap Object Pointer
// xxx1 : Fixnum (shifted)
// 0110 : Special (Null, True, False, Undef)

const uintptr_t TAG_MASK = 3;
const uintptr_t TAG_PTR = 0;
const uintptr_t TAG_FIXNUM = 1;

const uintptr_t SPECIAL_MASK = 0xF;
const uintptr_t TAG_SPECIAL = 6;  // ...0110

typedef void* ScmObj;

inline bool is_fixnum(ScmObj obj) { return ((uintptr_t)obj & TAG_MASK) == TAG_FIXNUM; }
inline bool is_ptr(ScmObj obj) { return ((uintptr_t)obj & TAG_MASK) == TAG_PTR && obj != NULL; }
inline intptr_t val_fixnum(ScmObj obj) { return (intptr_t)obj >> 2; }
inline ScmObj make_fixnum(intptr_t n) { return (ScmObj)((n << 2) | TAG_FIXNUM); }

// -- Special Objects --
#define SCM_NULL  ((ScmObj)0x06)
#define SCM_TRUE  ((ScmObj)0x16)
#define SCM_FALSE ((ScmObj)0x26)
#define SCM_EOF   ((ScmObj)0x36)
#define SCM_UNDEF ((ScmObj)0x46)

inline bool is_null(ScmObj obj) { return obj == SCM_NULL; }
inline bool is_true(ScmObj obj) { return obj == SCM_TRUE; }
inline bool is_false(ScmObj obj) { return obj == SCM_FALSE; }
inline bool is_eof(ScmObj obj) { return obj == SCM_EOF; }

// -- Heap Objects --

enum ObjType { T_PAIR, T_SYMBOL, T_STRING, T_PROC, T_ENV };

struct Header {
  ObjType type;
};

struct ScmPair : public Header {
  ScmObj car;
  ScmObj cdr;
};

struct ScmSymbol : public Header {
  char* name;  // dynamically allocated, but manual linkage?
               // Ideally we should allocate name_str in GC heap too or use std::string but that's complex for simple GC.
               // For this simple example, we'll malloc name and free in finalize.
};

struct ScmString : public Header {
  char* value;
};

typedef ScmObj (*PrimProc)(int argc, ScmObj* argv);

struct ScmProc : public Header {
  bool is_primitive;
  union {
    PrimProc prim;
    struct {
      ScmObj params;
      ScmObj body;
      ScmObj env;
    } func;
  };
};

// -- Allocation helpers --

ScmObj alloc_obj(size_t size, ObjType type) {
  void* ptr = NULL;
  if (size <= 16) {
    ptr = slab_16.new_collectible_object();
  } else if (size <= 32) {
    ptr = slab_32.new_collectible_object();
  } else {
    fatal("Unsupported object size: %lu", size);
  }
  ((Header*)ptr)->type = type;
  return (ScmObj)ptr;
}

ScmObj cons(ScmObj car, ScmObj cdr) {
  ScmPair* p = (ScmPair*)alloc_obj(sizeof(ScmPair), T_PAIR);
  p->car = car;
  p->cdr = cdr;
  heap.write_barrier(p);
  return (ScmObj)p;
}

ScmObj make_symbol(const char* s) {
  // Simple internship could be done here, but skipping for minimal implementation
  ScmSymbol* sym = (ScmSymbol*)alloc_obj(sizeof(ScmSymbol), T_SYMBOL);
  sym->name = strdup(s);
  return (ScmObj)sym;
}

ScmObj make_string(const char* s) {
  ScmString* str = (ScmString*)alloc_obj(sizeof(ScmString), T_STRING);
  str->value = strdup(s);
  return (ScmObj)str;
}

ScmObj make_prim(PrimProc fn) {
  ScmProc* p = (ScmProc*)alloc_obj(sizeof(ScmProc), T_PROC);
  p->is_primitive = true;
  p->prim = fn;
  return (ScmObj)p;
}

ScmObj make_closure(ScmObj params, ScmObj body, ScmObj env) {
  ScmProc* p = (ScmProc*)alloc_obj(sizeof(ScmProc), T_PROC);
  p->is_primitive = false;
  p->func.params = params;
  p->func.body = body;
  p->func.env = env;
  heap.write_barrier(p);
  return (ScmObj)p;
}

// -- Accessors --

inline ObjType type_of(ScmObj obj) { return ((Header*)obj)->type; }
inline bool is_pair(ScmObj obj) { return is_ptr(obj) && type_of(obj) == T_PAIR; }
inline bool is_symbol(ScmObj obj) { return is_ptr(obj) && type_of(obj) == T_SYMBOL; }
inline bool is_string(ScmObj obj) { return is_ptr(obj) && type_of(obj) == T_STRING; }
inline bool is_proc(ScmObj obj) { return is_ptr(obj) && type_of(obj) == T_PROC; }

#define CAR(obj) (((ScmPair*)(obj))->car)
#define CDR(obj) (((ScmPair*)(obj))->cdr)

// -- GC Callbacks --

void trace_obj(void* obj) {
  if (!is_ptr(obj)) return;
  Header* h = (Header*)obj;
  switch (h->type) {
    case T_PAIR: {
      ScmPair* p = (ScmPair*)h;
      if (is_ptr(p->car)) heap.shade(p->car);
      if (is_ptr(p->cdr)) heap.shade(p->cdr);
      break;
    }
    case T_PROC: {
      ScmProc* p = (ScmProc*)h;
      if (!p->is_primitive) {
        if (is_ptr(p->func.params)) heap.shade(p->func.params);
        if (is_ptr(p->func.body)) heap.shade(p->func.body);
        if (is_ptr(p->func.env)) heap.shade(p->func.env);
      }
      break;
    }
    default:
      break;
  }
}

void finalize_obj(void* obj) {
  if (!is_ptr(obj)) return;
  Header* h = (Header*)obj;
  if (h->type == T_SYMBOL) {
    free(((ScmSymbol*)h)->name);
  } else if (h->type == T_STRING) {
    free(((ScmString*)h)->value);
  }
}

// -- Root Protection --

vector<ScmObj*> root_stack;

struct GCProtect {
  GCProtect(ScmObj& obj) { root_stack.push_back(&obj); }
  ~GCProtect() { root_stack.pop_back(); }
};

// -- Symbol Table (Global) --
// Simple list of symbols for internship? Or just assume distinct?
// For a REPL, we need interned symbols to match variables.
ScmObj interned_symbols = SCM_NULL;

ScmObj intern(const char* name) {
  GCProtect gc(interned_symbols);
  ScmObj p = interned_symbols;
  while (!is_null(p)) {
    ScmObj sym = CAR(p);
    if (strcmp(((ScmSymbol*)sym)->name, name) == 0) return sym;
    p = CDR(p);
  }
  ScmObj new_sym = make_symbol(name);
  interned_symbols = cons(new_sym, interned_symbols);
  return new_sym;
}

// -- Environment --

ScmObj global_env = SCM_NULL;

ScmObj extend_env(ScmObj vars, ScmObj vals, ScmObj base_env) { return cons(cons(vars, vals), base_env); }

ScmObj lookup_var(ScmObj var, ScmObj env) {
  while (!is_null(env)) {
    ScmObj frame = CAR(env);
    ScmObj vars = CAR(frame);
    ScmObj vals = CDR(frame);
    while (!is_null(vars)) {
      if (CAR(vars) == var) return CAR(vals);
      vars = CDR(vars);
      vals = CDR(vals);
    }
    env = CDR(env);
  }
  printf("Error: Unbound variable: %s\n", ((ScmSymbol*)var)->name);
  return SCM_UNDEF;
}

void define_var(ScmObj var, ScmObj val, ScmObj* env_ptr) {
  ScmObj env = *env_ptr;
  if (is_null(env)) {
    // Create new frame
    *env_ptr = extend_env(cons(var, SCM_NULL), cons(val, SCM_NULL), SCM_NULL);
    return;
  }
  ScmObj frame = CAR(env);
  ScmObj vars = CAR(frame);
  ScmObj vals = CDR(frame);

  // Check if exists in first frame
  while (!is_null(vars)) {
    if (CAR(vars) == var) {
      // Update
      ScmPair* vpair = (ScmPair*)vals;  // risky cast if not pair but it must be
      vpair->car = val;
      heap.write_barrier(vpair);
      return;
    }
    vars = CDR(vars);
    vals = CDR(vals);
  }

  // Add to first frame
  ScmPair* f = (ScmPair*)frame;
  f->car = cons(var, f->car);
  f->cdr = cons(val, f->cdr);
  heap.write_barrier(f);
}

// -- Reader --

bool is_delimiter(int c) { return isspace(c) || c == EOF || c == '(' || c == ')' || c == '"' || c == ';'; }

int peek(istream& in) {
  int c = in.peek();
  while (isspace(c) || c == ';') {
    if (c == ';') {
      while (c != '\n' && c != EOF) c = in.get();
    } else {
      in.get();
    }
    c = in.peek();
  }
  return c;
}

ScmObj read(istream& in) {
  int c = peek(in);
  if (c == EOF) return SCM_EOF;

  if (c == '(') {
    in.get();
    c = peek(in);
    if (c == ')') {
      in.get();
      return SCM_NULL;
    }
    ScmObj head = read(in);
    GCProtect gc_head(head);  // PROTECT head
    ScmObj tail = SCM_NULL;
    ScmObj* tail_ptr = &tail;  // To build list efficiently

    // This is a simplified list reader, proper one handles dot
    ScmObj list = cons(head, SCM_NULL);
    ScmObj p = list;
    GCProtect gc1(list);
    // p is interior pointer, but list protects the whole structure if p is part of it.
    // However, if p moves (it doesn't in non-moving GC), we are fine.
    // p itself is not a root if we don't register it, but list is registered.

    while (true) {
      c = peek(in);
      if (c == ')') {
        in.get();
        break;
      }
      if (c == EOF) {
        printf("Error: Unexpected EOF in list\n");
        return SCM_EOF;
      }
      ScmObj elem = read(in);
      GCProtect gc_elem(elem);  // PROTECT elem
      ScmPair* pp = (ScmPair*)p;
      pp->cdr = cons(elem, SCM_NULL);
      heap.write_barrier(pp);
      p = CDR(p);
    }
    return list;
  } else if (isdigit(c) || (c == '-' && isdigit(in.peek()))) {
    // Number
    string buf;
    buf += (char)in.get();
    while (isdigit(in.peek())) buf += (char)in.get();
    return make_fixnum(stol(buf));
  } else {
    // Symbol
    string buf;
    while (!is_delimiter(in.peek())) buf += (char)in.get();
    if (buf == "#t") return SCM_TRUE;
    if (buf == "#f") return SCM_FALSE;
    return intern(buf.c_str());
  }
  return SCM_EOF;
}

// -- Printer --

void print(ScmObj obj) {
  if (is_fixnum(obj)) {
    cout << val_fixnum(obj);
  } else if (is_null(obj)) {
    cout << "()";
  } else if (is_true(obj))
    cout << "#t";
  else if (is_false(obj))
    cout << "#f";
  else if (is_eof(obj))
    cout << "#<eof>";
  else if (obj == SCM_UNDEF)
    cout << "#<undef>";
  else if (is_string(obj))
    cout << "\"" << ((ScmString*)obj)->value << "\"";
  else if (is_symbol(obj))
    cout << ((ScmSymbol*)obj)->name;
  else if (is_pair(obj)) {
    cout << "(";
    print(CAR(obj));
    ScmObj p = CDR(obj);
    while (is_pair(p)) {
      cout << " ";
      print(CAR(p));
      p = CDR(p);
    }
    if (!is_null(p)) {
      cout << " . ";
      print(p);
    }
    cout << ")";
  } else if (is_proc(obj)) {
    ScmProc* p = (ScmProc*)obj;
    if (p->is_primitive)
      cout << "#<prim>";
    else
      cout << "#<closure>";
  } else {
    cout << "#<unknown>";
  }
}

// -- Evaluator --

ScmObj eval(ScmObj exp, ScmObj env);

ScmObj eval_list(ScmObj args, ScmObj env) {
  if (is_null(args)) return SCM_NULL;
  return cons(eval(CAR(args), env), eval_list(CDR(args), env));
}

ScmObj eval(ScmObj exp, ScmObj env) {
  if (is_symbol(exp)) return lookup_var(exp, env);
  if (!is_pair(exp)) return exp;  // Self-evaluating

  ScmObj op = CAR(exp);
  ScmObj args = CDR(exp);

  if (is_symbol(op)) {
    const char* s = ((ScmSymbol*)op)->name;
    if (strcmp(s, "quote") == 0) return CAR(args);
    if (strcmp(s, "if") == 0) {
      ScmObj test = eval(CAR(args), env);
      if (test != SCM_FALSE)
        return eval(CAR(CDR(args)), env);
      else {
        if (!is_null(CDR(CDR(args)))) return eval(CAR(CDR(CDR(args))), env);
        return SCM_UNDEF;
      }
    }
    if (strcmp(s, "lambda") == 0) {
      return make_closure(CAR(args), CDR(args), env);  // (lambda (params...) body...)
    }
    if (strcmp(s, "define") == 0) {
      ScmObj var = CAR(args);
      ScmObj val = eval(CAR(CDR(args)), env);
      define_var(var, val, &global_env);
      return var;
    }
  }

  ScmObj proc = eval(op, env);
  ScmObj vals = eval_list(args, env);
  GCProtect gc1(proc);
  GCProtect gc2(vals);

  if (is_proc(proc)) {
    ScmProc* p = (ScmProc*)proc;
    if (p->is_primitive) {
      // Convert list to argv
      vector<ScmObj> vargs;
      while (!is_null(vals)) {
        vargs.push_back(CAR(vals));
        vals = CDR(vals);
      }
      return p->prim(vargs.size(), vargs.data());
    } else {
      // Apply closure
      ScmObj new_env = extend_env(p->func.params, vals, p->func.env);
      ScmObj body = p->func.body;
      ScmObj res = SCM_UNDEF;
      while (!is_null(body)) {
        res = eval(CAR(body), new_env);
        body = CDR(body);
      }
      return res;
    }
  }

  printf("Error: Not a procedure: ");
  print(proc);
  cout << endl;
  return SCM_UNDEF;
}

// -- Primitives --

ScmObj p_add(int argc, ScmObj* argv) {
  intptr_t sum = 0;
  for (int i = 0; i < argc; i++) {
    if (!is_fixnum(argv[i])) return SCM_FALSE;  // Error handling simplified
    sum += val_fixnum(argv[i]);
  }
  return make_fixnum(sum);
}

ScmObj p_sub(int argc, ScmObj* argv) {
  if (argc == 0) return make_fixnum(0);
  intptr_t acc = val_fixnum(argv[0]);
  if (argc == 1) return make_fixnum(-acc);
  for (int i = 1; i < argc; i++) acc -= val_fixnum(argv[i]);
  return make_fixnum(acc);
}

ScmObj p_cons(int argc, ScmObj* argv) { return cons(argv[0], argv[1]); }

ScmObj p_car(int argc, ScmObj* argv) { return CAR(argv[0]); }

ScmObj p_cdr(int argc, ScmObj* argv) { return CDR(argv[0]); }

ScmObj p_eq(int argc, ScmObj* argv) { return (argv[0] == argv[1]) ? SCM_TRUE : SCM_FALSE; }

ScmObj p_null(int argc, ScmObj* argv) { return is_null(argv[0]) ? SCM_TRUE : SCM_FALSE; }

void init_primitives() {
  define_var(intern("+"), make_prim(p_add), &global_env);
  define_var(intern("-"), make_prim(p_sub), &global_env);
  define_var(intern("cons"), make_prim(p_cons), &global_env);
  define_var(intern("car"), make_prim(p_car), &global_env);
  define_var(intern("cdr"), make_prim(p_cdr), &global_env);
  define_var(intern("eq?"), make_prim(p_eq), &global_env);
  define_var(intern("null?"), make_prim(p_null), &global_env);
}

// -- Main --

void root_snapshot() {
  // Traverse roots
  if (is_ptr(interned_symbols)) heap.shade(interned_symbols);
  if (is_ptr(global_env)) heap.shade(global_env);
  for (ScmObj* p : root_stack) {
    if (is_ptr(*p)) heap.shade(*p);
  }
}

int main(int argc, char** argv) {
  pool.init(64 * 1024 * 1024, 4 * 1024 * 1024);  // 64MB pool
  heap.init(&pool);
  slab_16.init(&heap, 16, true, true);
  slab_32.init(&heap, 32, true, true);

  heap.set_trace_proc(trace_obj);
  heap.set_finalize_proc(finalize_obj);
  heap.set_snapshot_root_proc(root_snapshot);
  // We need other callbacks to avoid fatal errors
  heap.set_clear_trip_bytes_proc([]() {});
  heap.set_update_weak_reference_proc([]() {});

  init_primitives();

  cout << "Scheme REPL (Concurrent GC)" << endl;
  while (true) {
    cout << "> ";
    ScmObj exp = read(cin);
    GCProtect gc1(exp);
    if (is_eof(exp)) break;
    ScmObj res = eval(exp, global_env);
    GCProtect gc2(res);
    print(res);
    cout << endl;

    // Trigger GC occasionally? Or rely on background thread?
    // Concurrent GC is running in background.
  }

  heap.terminate();
  pool.destroy();
  return 0;
}
