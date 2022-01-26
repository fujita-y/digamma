// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arith.h"
#include "codegen.h"
#include "printer.h"
#include "violation.h"
#include "vm.h"

#define FOLD_TAIL_CALL_TRACE 1
#define UNWRAP_BACKTRACE     1

#define STACKP(p)            (((p) >= (void*)m_stack_top) & ((p) < (void*)m_stack_limit))
#define FORWARDP(p)          ((*(uintptr_t*)(p)) & 1)
#define FORWARD(from, to)    ((*(uintptr_t*)(from)) = ((uintptr_t)(to) | 1))
#define RESOLVE(p)           ((void*)((*(uintptr_t*)(p)) & (~1)))

#define NATIVE_THUNK_POST_DISPATCH(_N_)            \
  {                                                \
    switch (n) {                                   \
      case native_thunk_pop_cont:                  \
        goto pop_cont;                             \
      case native_thunk_apply:                     \
        goto apply;                                \
      case native_thunk_loop:                      \
        goto loop;                                 \
      case native_thunk_resume_loop:               \
        goto RESUME_LOOP;                          \
      case native_thunk_escape:                    \
        return;                                    \
      default:                                     \
        fatal("unsupported thunk protocol %d", n); \
    }                                              \
  }

static void object_copy(void* dst, const void* src, intptr_t bsize) {
  assert(bsize % sizeof(scm_obj_t) == 0);
  intptr_t c = bsize / sizeof(scm_obj_t);
  for (intptr_t i = 0; i < c; i++) ((scm_obj_t*)dst)[i] = ((scm_obj_t*)src)[i];
}

void* VM::save_cont(void* lnk) {
  if (!STACKP(lnk)) return lnk;
  void* up = save_cont(*(void**)lnk);
  vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
  cont->env = save_env(cont->env);
  intptr_t asize = (intptr_t)cont - (intptr_t)cont->fp;
  intptr_t fsize = asize + sizeof(vm_cont_rec_t);
  void* heap_top = new_heapcont_rec(m_heap, fsize);
  object_copy(heap_top, cont->fp, fsize);
  vm_cont_t heap_cont = (vm_cont_t)((intptr_t)heap_top + asize);
  heap_cont->up = up;
  heap_cont->fp = (scm_obj_t*)heap_top;
  return &heap_cont->up;
}

void VM::save_stack() {
  intptr_t argc = m_sp - m_fp;
  m_cont = save_cont(m_cont);
  m_env = save_env(m_env);
  update_cont(m_cont);
  memmove(m_stack_top, m_fp, sizeof(scm_obj_t) * argc);
  m_fp = m_stack_top;
  m_sp = m_stack_top + argc;
}

void* VM::gc_env(void* lnk) {
  if (!STACKP(lnk)) return lnk;
  if (FORWARDP(lnk)) return RESOLVE(lnk);
  void* up = gc_env(*(void**)lnk);
  vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
  intptr_t bytes = env->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
  object_copy(m_fp, (scm_obj_t*)env - env->count, bytes);
  vm_env_t to_env = (vm_env_t)(m_fp + env->count);
  m_fp = (scm_obj_t*)((intptr_t)m_fp + bytes);
  to_env->up = up;
  FORWARD(&env->up, &to_env->up);
  return &to_env->up;
}

void* VM::gc_cont(void* lnk) {
  if (!STACKP(lnk)) return lnk;
  void* up = gc_cont(*(void**)lnk);
  vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
  cont->env = gc_env(cont->env);
  intptr_t bytes = (intptr_t)cont - (intptr_t)cont->fp + sizeof(vm_cont_rec_t);
  object_copy(m_fp, cont->fp, bytes);
  m_fp = (scm_obj_t*)((intptr_t)m_fp + bytes);
  vm_cont_t to_cont = (vm_cont_t)((intptr_t)m_fp - sizeof(vm_cont_rec_t));
  to_cont->up = up;
  to_cont->fp = (scm_obj_t*)((intptr_t)m_fp - bytes);
  return &to_cont->up;
}

void VM::collect_stack(intptr_t acquire) {
  if (m_stack_busy) {
    save_stack();
    if (m_flags.collect_stack_notify != scm_false) {
      scoped_lock lock(m_current_output->lock);
      printer_t prt(this, m_current_output);
      prt.format("~&;; [collect-stack: store*]~%~!");
    }
    if ((uintptr_t)m_sp + acquire > (uintptr_t)m_stack_limit) {
      backtrace(m_current_error);
      fatal("fatal: vm stack overflow: can not handle more than %ld arguments under current configuration",
            (m_stack_limit - m_stack_top) - sizeof(vm_env_rec_t) / sizeof(scm_obj_t));
    }
    m_stack_busy = false;
#if STDEBUG
    check_vm_state();
#endif
    if (m_heap->m_stop_the_world) stop();
    return;
  }
  intptr_t argc = m_sp - m_fp;
  m_fp = m_to_stack_top;
  m_cont = gc_cont(m_cont);
  m_env = gc_env(m_env);
  object_copy(m_fp, m_sp - argc, sizeof(scm_obj_t) * argc);
  m_sp = m_fp + argc;
  scm_obj_t* tmp;
  tmp = m_stack_top;
  m_stack_top = m_to_stack_top;
  m_to_stack_top = tmp;
  tmp = m_stack_limit;
  m_stack_limit = m_to_stack_limit;
  m_to_stack_limit = tmp;
  if ((uintptr_t)m_sp + acquire >= (uintptr_t)m_stack_limit) {
    save_stack();
    if (m_flags.collect_stack_notify != scm_false) {
      scoped_lock lock(m_current_output->lock);
      printer_t prt(this, m_current_output);
      prt.format("~&;; [collect-stack: store**]~%~!");
    }
    m_stack_busy = true;
  } else {
    if (m_flags.collect_stack_notify != scm_false) {
      char buf[16];
      double rate = 1.0 - ((double)(m_sp - m_stack_top) / (double)(m_stack_limit - m_stack_top));
      snprintf(buf, sizeof(buf), "%.1lf%%", rate * 100.0);
      scoped_lock lock(m_current_output->lock);
      printer_t prt(this, m_current_output);
      prt.format("~&;; [collect-stack: %s free]~%~!", buf);
    }
    m_stack_busy = (m_sp - m_stack_top) > VM_STACK_BUSY_THRESHOLD(m_stack_limit - m_stack_top);
  }
#ifndef NDEBUG
  if ((uintptr_t)m_sp + acquire > (uintptr_t)m_stack_limit) {
    backtrace(m_current_error);
    fatal("%s:%u stack overflow", __FILE__, __LINE__);
  }
#endif
#if STDEBUG
  check_vm_state();
#endif
  if (m_heap->m_stop_the_world) stop();
}

void* VM::save_env(void* root) {
  vm_env_t current;
  vm_env_t env;
  if (STACKP(root)) {
    if (FORWARDP(root)) return RESOLVE(root);
    current = (vm_env_t)((intptr_t)root - offsetof(vm_env_rec_t, up));
    env = current;
    intptr_t bytes = env->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
    scm_obj_t* stack = (scm_obj_t*)env - env->count;
    scm_obj_t* heap = (scm_obj_t*)new_heapenv_rec(m_heap, bytes);
    assert(bytes % sizeof(scm_obj_t) == 0);
    intptr_t c = bytes / sizeof(scm_obj_t);
    for (intptr_t i = 0; i < c; i++) heap[i] = stack[i];
    intptr_t offset = (intptr_t)heap - (intptr_t)stack;
    root = (void*)((intptr_t)root + offset);
    env = (vm_env_t)((intptr_t)env + offset);
  } else {
    return root;
  }
  while (STACKP(env->up)) {
    if (FORWARDP(env->up)) {
      env->up = RESOLVE(env->up);
      break;
    }
    vm_env_t parent = (vm_env_t)((intptr_t)env->up - offsetof(vm_env_rec_t, up));
    intptr_t bytes = parent->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
    scm_obj_t* stack = (scm_obj_t*)parent - parent->count;
    scm_obj_t* heap = (scm_obj_t*)new_heapenv_rec(m_heap, bytes);
    assert(bytes % sizeof(scm_obj_t) == 0);
    intptr_t c = bytes / sizeof(scm_obj_t);
    for (intptr_t i = 0; i < c; i++) heap[i] = stack[i];
    intptr_t offset = (intptr_t)heap - (intptr_t)stack;
    vm_env_t heap_env = (vm_env_t)((intptr_t)parent + offset);
    FORWARD(&parent->up, &heap_env->up);
    env->up = &heap_env->up;
    env = heap_env;
  }
  FORWARD(&current->up, root);
  return root;
}

void VM::update_cont(void* lnk) {
  while (STACKP(lnk)) {
    vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
    if (cont->env && FORWARDP(cont->env)) cont->env = RESOLVE(cont->env);
    lnk = (*(void**)lnk);
  }
}

scm_obj_t* VM::lookup_iloc(scm_obj_t operands) {
  void* lnk = m_env;
  intptr_t level = FIXNUM(CAR(operands));
  while (level) {
    lnk = *(void**)lnk;
    level = level - 1;
  }
  vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
  return (scm_obj_t*)env - env->count + FIXNUM(CDR(operands));
}

#define CASE(code) case code:
#define SWITCH()   switch (instruction_to_opcode(CAAR(m_pc)))
#define OPERANDS   (CDAR(m_pc))

enum {
  apply_apply_trace_n_loop = 0,   // goto trace_n_loop;
  apply_apply_pop_cont,           // goto pop_cont;
  apply_apply_wrong_number_args,  // goto ERROR_APPLY_WRONG_NUMBER_ARGS
  apply_apply_bad_last_args       // goto ERROR_PROC_APPLY_BAD_LAST_ARGS
};

int VM::apply_apply_closure(scm_obj_t lastarg) {
  scm_closure_t closure = (scm_closure_t)m_value;
  if (HDR_CLOSURE_ARGS(closure->hdr) < 0) {
    int last_argc = safe_list_length(lastarg);
    if (last_argc >= 0) {
      intptr_t argc = m_sp - m_fp;
      intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
      args = -args - 1;
      if (argc == args) {
        if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
        m_sp[0] = list_copy(m_heap, lastarg);
        m_sp++;
      } else if (argc > args) {
        scm_obj_t lst = list_copy(m_heap, lastarg);
        do {
          lst = make_pair(m_heap, m_sp[-1], lst);
          m_sp--;
        } while (--argc > args);
        m_sp[0] = lst;
        m_sp++;
      } else if (argc + last_argc >= args) {
        scm_obj_t lst = lastarg;
        do {
          if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
          m_sp[0] = CAR(lst);
          m_sp++;
          lst = CDR(lst);
        } while (++argc < args);
        if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
        m_sp[0] = list_copy(m_heap, lst);
        m_sp++;
      } else {
        return apply_apply_wrong_number_args;
      }
      if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) >= (uintptr_t)m_stack_limit) collect_stack(sizeof(vm_env_rec_t));
      vm_env_t env = (vm_env_t)m_sp;
      env->count = args + 1;
      env->up = closure->env;
      m_sp = m_fp = (scm_obj_t*)(env + 1);
      m_pc = closure->pc;
      m_env = &env->up;
      return apply_apply_trace_n_loop;
    }
    return apply_apply_bad_last_args;
  }
  scm_obj_t lst = lastarg;
  while (PAIRP(lst)) {
    if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
    m_sp[0] = CAR(lst);
    m_sp++;
    lst = CDR(lst);
  }
  if (lst == scm_nil) {
    if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) >= (uintptr_t)m_stack_limit) collect_stack(sizeof(vm_env_rec_t));
    intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
    if (m_sp - m_fp != args) return apply_apply_wrong_number_args;
    vm_env_t env = (vm_env_t)m_sp;
    env->count = args;
    env->up = closure->env;
    m_sp = m_fp = (scm_obj_t*)(env + 1);
    m_pc = closure->pc;
    m_env = &env->up;
    return apply_apply_trace_n_loop;
  }
  return apply_apply_bad_last_args;
}

int VM::apply_apply_subr(scm_obj_t lastarg) {
  scm_obj_t lst = lastarg;
  while (PAIRP(lst)) {
    if (m_sp >= m_stack_limit) {
      if (m_fp == m_stack_top) {
        int argc = VM_STACK_BYTESIZE / sizeof(scm_obj_t);
        int more = safe_list_length(lst);
        if (more >= 0) {
          argc = argc + more;
          scm_subr_t subr = (scm_subr_t)m_value;
          scm_vector_t argv = make_vector(m_heap, argc, scm_unspecified);
          memcpy(argv->elts, m_stack_top, VM_STACK_BYTESIZE);
          for (int i = VM_STACK_BYTESIZE / sizeof(scm_obj_t); i < argc; i++) {
            argv->elts[i] = CAR(lst);
            lst = CDR(lst);
          }
          m_value = argv;
          m_fp = m_sp = m_stack_top;
          m_value = (*subr->adrs)(this, argc, argv->elts);
          if (m_value == scm_undef) {
            m_sp = m_fp;
            m_pc = CDR(m_pc);
            return apply_apply_trace_n_loop;
          }
          return apply_apply_pop_cont;
        }
        return apply_apply_bad_last_args;
      }
      collect_stack(sizeof(scm_obj_t));
    }
    m_sp[0] = CAR(lst);
    m_sp++;
    lst = CDR(lst);
  }
  if (lst == scm_nil) {
    scm_subr_t subr = (scm_subr_t)m_value;
    intptr_t argc = m_sp - m_fp;
    m_value = (*subr->adrs)(this, argc, m_fp);
    if (m_value == scm_undef) {
      m_sp = m_fp;
      m_pc = CDR(m_pc);
      return apply_apply_trace_n_loop;
    }
    return apply_apply_pop_cont;
  }
  return apply_apply_bad_last_args;
}

// subr_*() return state if it calls apply_scheme() or call_scheme() for implementation
//
// function        m_value    CAR(m_pc)
// ------------------------------------------
// apply_scheme()  scm_undef  scm_unspecified
// call_scheme()   scm_undef  scm_false

void VM::run() {
  bool resume = false;
again:
  try {
    loop(resume);
  } catch (vm_continue_t& e) {
    resume = true;
    goto again;
  } catch (...) {
    throw;
  }
}

void VM::loop(bool resume) {
  scm_obj_t operand_trace;
  scm_obj_t obj;
  assert(PAIRP(m_pc));
  if (resume) goto pop_cont;
  goto loop;

apply : {
  if (CLOSUREP(m_value)) {
    if (m_heap->m_stop_the_world) stop();
    if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
      scm_closure_t closure = (scm_closure_t)m_value;
      intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
      if (m_sp - m_fp != args) goto APPLY_VARIADIC;
      vm_env_t env = (vm_env_t)m_sp;
      env->count = args;
      env->up = closure->env;
      m_sp = m_fp = (scm_obj_t*)(env + 1);
      m_pc = closure->pc;
      m_env = &env->up;
      if (closure->code) {
        intptr_t (*thunk)(intptr_t) = (intptr_t(*)(intptr_t))closure->code;
        intptr_t n = (*thunk)((intptr_t)this);
        NATIVE_THUNK_POST_DISPATCH(n);
      }
      goto trace_n_loop;
    }
    goto COLLECT_STACK_ENV_REC_N_APPLY;
  }
  if (SUBRP(m_value)) {
    scm_subr_t subr = (scm_subr_t)m_value;
    intptr_t argc = m_sp - m_fp;
    m_value = (*subr->adrs)(this, argc, m_fp);
    if (m_value == scm_undef) goto BACK_TO_TRACE_N_LOOP;
    goto pop_cont;
  }
}
  goto APPLY_SPECIAL;

pop_cont : {
  if (m_cont == NULL) return;
  vm_cont_t cont = (vm_cont_t)((intptr_t)m_cont - offsetof(vm_cont_rec_t, up));
  m_trace = cont->trace;
  m_fp = cont->fp;
  m_pc = cont->pc;
  m_env = cont->env;
  m_cont = cont->up;
  if (STACKP(cont)) {
    m_sp = (scm_obj_t*)cont;
  } else {
    intptr_t nargs = (scm_obj_t*)cont - (scm_obj_t*)cont->fp;
    const scm_obj_t* s = (scm_obj_t*)cont->fp;
    scm_obj_t* d = (scm_obj_t*)m_stack_top;
    for (intptr_t i = 0; i < nargs; i++) d[i] = s[i];
    m_fp = m_stack_top;
    m_sp = m_fp + nargs;
  }
  m_trace_tail = scm_unspecified;
  if (m_heap->m_stop_the_world) stop();
  if (cont->code != NULL) {
    intptr_t (*thunk)(intptr_t) = (intptr_t(*)(intptr_t))cont->code;
    intptr_t n = (*thunk)((intptr_t)this);
    NATIVE_THUNK_POST_DISPATCH(n);
  }
}
  goto loop;

trace_n_loop:
  if (operand_trace != scm_nil) {
    if (m_trace == scm_unspecified)
      m_trace = operand_trace;
    else
      m_trace_tail = operand_trace;

#ifndef NDEBUG
  #if 0
            {
                if (CDR(operand_trace) == scm_nil) {
                    // no info
                } else if (FIXNUMP(CDR(operand_trace))) {
                    // (path . fixnum) : loaded form
                    assert(STRINGP(CAR(operand_trace)));
                    scm_string_t string = (scm_string_t)CAR(operand_trace);
                    intptr_t comment = FIXNUM(CDR(operand_trace));
                    intptr_t line = comment / MAX_SOURCE_COLUMN;
                    intptr_t column = comment % MAX_SOURCE_COLUMN;
                    scoped_lock lock(m_current_output->lock);
                    printer_t(this, m_current_output).format("trace: %s line %ld column %ld~%~!", string->name, line, column);
                } else {
                    // (expr path . fixnum) : repl form
                    scm_string_t string = (scm_string_t)CADR(operand_trace);
                    intptr_t comment = FIXNUM(CDDR(operand_trace));
                    intptr_t line = comment / MAX_SOURCE_COLUMN;
                    scoped_lock lock(m_current_output->lock);
                    printer_t(this, m_current_output).format("trace: ~s  ... %s line %ld~%~!", CAR(operand_trace), string->name, line);
                }
            }
  #endif
#endif
  }
  goto loop;

loop:
  assert(m_sp <= m_stack_limit);
  assert(OPCODESYMBOLP(CAAR(m_pc)));
#if PROFILE_OPCODE
  {
    static int last_opecode;
    int opcode = instruction_to_opcode(CAAR(m_pc));
    if (m_opcode_profile[opcode].count < UINT64_MAX) {
      m_opcode_profile[opcode].count++;
    }
    if (m_opcode_profile[opcode].prev[last_opecode] < UINT64_MAX) {
      m_opcode_profile[opcode].prev[last_opecode]++;
    }
    last_opecode = opcode;
  }
#endif
  SWITCH() {
    CASE(VMOP_IF_FALSE_CALL) {
      if (m_value == scm_false) goto ENT_VMOP_CALL;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_CALL) ENT_VMOP_CALL : {
      if ((uintptr_t)m_sp + sizeof(vm_cont_rec_t) < (uintptr_t)m_stack_limit) {
#if STDEBUG
        check_vm_state();
#endif
        vm_cont_t cont = (vm_cont_t)m_sp;
        cont->trace = m_trace;
        cont->fp = m_fp;
        cont->pc = CDR(m_pc);
        cont->code = NULL;
        cont->env = m_env;
        cont->up = m_cont;
        m_sp = m_fp = (scm_obj_t*)(cont + 1);
        m_cont = &cont->up;
        m_pc = OPERANDS;
        m_trace = m_trace_tail = scm_unspecified;
#if STDEBUG
        check_vm_state();
#endif
        goto loop;
      }
      goto COLLECT_STACK_CONT_REC;
    }

    CASE(VMOP_RET_GLOC) {
      assert(GLOCP(OPERANDS));
      m_value = ((scm_gloc_t)OPERANDS)->value;
      if (m_value == scm_undef) goto ERROR_RET_GLOC;
      goto pop_cont;
    }

    CASE(VMOP_RET_CONST) {
      m_value = OPERANDS;
      goto pop_cont;
    }

    CASE(VMOP_RET_ILOC) {
      m_value = *lookup_iloc(OPERANDS);
      if (m_value == scm_undef) goto ERROR_RET_ILOC;
      goto pop_cont;
    }

    CASE(VMOP_PUSH_GLOC) {
      if (m_sp < m_stack_limit) {
        assert(GLOCP(OPERANDS));
        scm_gloc_t gloc = (scm_gloc_t)OPERANDS;
        scm_obj_t value = gloc->value;
        if (value == scm_undef) goto ERROR_PUSH_GLOC;
        m_sp[0] = value;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_SUBR) {
      scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
#if PROFILE_SUBR
      subr->c_push++;
#endif
      assert(SUBRP(subr));
      intptr_t argc = FIXNUM(CADR(OPERANDS));
      assert(argc > 0);
      m_value = (*subr->adrs)(this, argc, m_sp - argc);
      m_sp = m_sp - argc;
      assert(m_sp >= m_fp);
      if (m_value != scm_undef) {
        m_sp[0] = m_value;
        m_sp++;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_PUSH_CAR_ILOC) {
      if (m_sp < m_stack_limit) {
        obj = *lookup_iloc(CAR(OPERANDS));
        if (PAIRP(obj)) {
          m_sp[0] = CAR(obj);
          m_sp++;
          m_pc = CDR(m_pc);
          goto loop;
        }
        goto ERROR_PUSH_CAR_ILOC;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_CDR_ILOC) {
      if (m_sp < m_stack_limit) {
        obj = *lookup_iloc(CAR(OPERANDS));
        if (PAIRP(obj)) {
          m_sp[0] = CDR(obj);
          m_sp++;
          m_pc = CDR(m_pc);
          goto loop;
        }
        goto ERROR_PUSH_CDR_ILOC;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_ILOC) {
      if (m_sp < m_stack_limit) {
        m_sp[0] = *lookup_iloc(OPERANDS);
        if (m_sp[0] == scm_undef) goto ERROR_LETREC_VIOLATION;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH) {
      if (m_sp < m_stack_limit) {
        m_sp[0] = m_value;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_CONST) {
      if (m_sp < m_stack_limit) {
        m_sp[0] = OPERANDS;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_ILOC1) {
      if (m_sp < m_stack_limit) {
        void* lnk = *(void**)m_env;
        vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
        m_sp[0] = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
        if (m_sp[0] == scm_undef) goto ERROR_LETREC_VIOLATION;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_ILOC0) {
      if (m_sp < m_stack_limit) {
        vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
        m_sp[0] = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
        if (m_sp[0] == scm_undef) goto ERROR_LETREC_VIOLATION;
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_APPLY_GLOC) {
      operand_trace = CDR(OPERANDS);
      assert(GLOCP(CAR(OPERANDS)));
      scm_gloc_t gloc = (scm_gloc_t)CAR(OPERANDS);
      m_value = gloc->value;
      if (m_value == scm_undef) goto ERROR_APPLY_GLOC;

#if ENABLE_COMPILE_GLOC
      if (m_codegen && CLOSUREP(gloc->value)) {
        scm_closure_t closure = (scm_closure_t)gloc->value;
        if (!HDR_CLOSURE_INSPECTED(closure->hdr)) {
          // printer_t prt(this, m_current_output);
          // prt.format("codegen: ~s~&", symbol);
          closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
          m_codegen->m_usage.globals++;
          m_codegen->compile(closure);
        }
      }
#endif

      goto apply;
    }

    CASE(VMOP_RET_SUBR) {
      operand_trace = CDR(OPERANDS);
      assert(SUBRP(CAR(OPERANDS)));
      scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
#if PROFILE_SUBR
      subr->c_apply++;
#endif
      intptr_t argc = m_sp - m_fp;
      m_value = (*subr->adrs)(this, argc, m_fp);
      assert(m_value != scm_undef || ((m_value == scm_undef) && (CAR(m_pc) == scm_unspecified)));
      if (m_value == scm_undef) goto BACK_TO_TRACE_N_LOOP;
      goto pop_cont;
    }

    CASE(VMOP_APPLY_ILOC) {
      operand_trace = CDR(OPERANDS);
      m_value = *lookup_iloc(CAR(OPERANDS));
      if (m_value == scm_undef) goto ERROR_APPLY_ILOC;
      goto apply;
    }

    CASE(VMOP_APPLY_ILOC_LOCAL) {
      if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
        operand_trace = CDR(OPERANDS);
        void* lnk = m_env;
        intptr_t level = FIXNUM(CAAR(OPERANDS));
        while (level) {
          lnk = *(void**)lnk;
          level = level - 1;
        }
        vm_env_t env2 = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
        scm_obj_t obj = *((scm_obj_t*)env2 - env2->count + FIXNUM(CDAR(OPERANDS)));
        vm_env_t env = (vm_env_t)m_sp;
        env->count = m_sp - m_fp;
        env->up = &env2->up;
        m_env = &env->up;
        m_sp = m_fp = (scm_obj_t*)(env + 1);
        m_pc = obj;
        goto trace_n_loop;
      }
      goto COLLECT_STACK_ENV_REC;
    }

    CASE(VMOP_APPLY) {
      operand_trace = OPERANDS;
#if STDEBUG
      check_vm_state();
#endif
      goto apply;
    }

    CASE(VMOP_EXTEND) {
      if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
        assert(FIXNUMP(OPERANDS));
        intptr_t argc = FIXNUM(OPERANDS);
        assert(argc == m_sp - m_fp);
        vm_env_t env = (vm_env_t)m_sp;
        env->count = argc;
        env->up = m_env;
        m_sp = m_fp = (scm_obj_t*)(env + 1);
        m_env = &env->up;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ENV_REC;
    }

    CASE(VMOP_EXTEND_ENCLOSE) {
      if ((uintptr_t)m_sp + sizeof(scm_obj_t) + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
        m_sp[0] = scm_undef;
        m_sp++;
        vm_env_t env = (vm_env_t)m_sp;
        env->count = 1;
        env->up = m_env;
        m_sp = m_fp = (scm_obj_t*)(env + 1);
        m_env = &env->up;
        m_env = save_env(m_env);
        update_cont(m_cont);
        env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
        scm_obj_t* slot = (scm_obj_t*)env - 1;
#if PREBIND_CLOSE
        *slot = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
#else
        scm_obj_t spec = CAR(OPERANDS);
        scm_obj_t code = CDR(OPERANDS);
        scm_obj_t doc = CDDR(spec);
        *slot = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
#endif
        m_pc = CDR(m_pc);
#if STDEBUG
        check_vm_state();
#endif
        goto loop;
      }
      goto COLLECT_STACK_ENV_REC_N_ONE;
    }

    CASE(VMOP_EXTEND_ENCLOSE_LOCAL) {
      if ((uintptr_t)m_sp + sizeof(scm_obj_t) + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
        m_sp[0] = CDR(OPERANDS);
        m_sp++;
        vm_env_t env = (vm_env_t)m_sp;
        env->count = 1;
        env->up = m_env;
        m_sp = m_fp = (scm_obj_t*)(env + 1);
        m_env = &env->up;
        m_pc = CDR(m_pc);
#if STDEBUG
        check_vm_state();
#endif
        goto loop;
      }
      goto COLLECT_STACK_ENV_REC_N_ONE;
    }

    CASE(VMOP_EXTEND_UNBOUND) {
      assert(FIXNUMP(OPERANDS));
      intptr_t argc = FIXNUM(OPERANDS);
      if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) + sizeof(scm_obj_t*) * argc < (uintptr_t)m_stack_limit) {
        for (intptr_t i = 0; i < argc; i++) {
          m_sp[0] = scm_undef;
          m_sp++;
        }
        vm_env_t env = (vm_env_t)m_sp;
        env->count = argc;
        env->up = m_env;
        m_sp = m_fp = (scm_obj_t*)(env + 1);
        m_env = &env->up;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ENV_REC_N_OPERAND;
    }

    CASE(VMOP_PUSH_CLOSE) {
      if (m_sp < m_stack_limit) {
        if (STACKP(m_env)) {
          m_env = save_env(m_env);
          update_cont(m_cont);
        }
#if PREBIND_CLOSE
        m_sp[0] = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
#else
        scm_obj_t spec = CAR(OPERANDS);
        scm_obj_t code = CDR(OPERANDS);
        scm_obj_t doc = CDDR(spec);
        m_sp[0] = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
#endif
        m_sp++;
        m_pc = CDR(m_pc);
#if STDEBUG
        check_vm_state();
#endif
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_CLOSE_LOCAL) {
      if (m_sp < m_stack_limit) {
        m_sp[0] = CDR(OPERANDS);
        m_sp++;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_ENCLOSE) {
      assert(FIXNUMP(OPERANDS));
      intptr_t argc = FIXNUM(OPERANDS);
      assert(m_env);
      vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
      scm_obj_t* dst = (scm_obj_t*)env - env->count;
      if (STACKP(env)) {
        for (intptr_t i = 0; i < argc; i++) dst[i] = m_fp[i];
      } else {
        for (intptr_t i = 0; i < argc; i++) {
          m_heap->write_barrier(m_fp[i]);
          dst[i] = m_fp[i];
        }
      }
      m_sp = m_fp;
      m_pc = CDR(m_pc);
#if STDEBUG
      check_vm_state();
#endif
      goto loop;
    }

    CASE(VMOP_GLOC) {
      m_value = ((scm_gloc_t)OPERANDS)->value;
      if (m_value == scm_undef) goto ERROR_GLOC;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_ILOC) {
      m_value = *lookup_iloc(OPERANDS);
      if (m_value == scm_undef) goto ERROR_LETREC_VIOLATION;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_CAR_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (PAIRP(obj)) {
        m_value = CAR(obj);
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto ERROR_CAR_ILOC;
    }

    CASE(VMOP_CDR_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (PAIRP(obj)) {
        m_value = CDR(obj);
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto ERROR_CDR_ILOC;
    }

    CASE(VMOP_CONST) {
      m_value = OPERANDS;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_SUBR) {
      scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
#if PROFILE_SUBR
      subr->c_load++;
#endif
      assert(SUBRP(subr));
      intptr_t argc = FIXNUM(CADR(OPERANDS));
      m_value = (*subr->adrs)(this, argc, m_sp - argc);
      m_sp = m_sp - argc;
      assert(m_sp >= m_fp);
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_ILOC1) {
      void* lnk = *(void**)m_env;
      vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
      m_value = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
      if (m_value == scm_undef) goto ERROR_LETREC_VIOLATION;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_ILOC0) {
      vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
      m_value = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
      if (m_value == scm_undef) goto ERROR_LETREC_VIOLATION;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_TRUE) {
      if (m_value != scm_false) {
        m_pc = OPERANDS;
        goto loop;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NULLP_RET_CONST) {
      if (m_value == scm_nil) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_EQP) {
      m_sp--;
      assert(m_sp >= m_fp);
      if (m_sp[0] == m_value) {
        m_pc = OPERANDS;
        goto loop;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NULLP) {
      if (m_value == scm_nil) {
        m_pc = OPERANDS;
        goto loop;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_PAIRP) {
      if (PAIRP(m_value)) {
        m_pc = OPERANDS;
        goto loop;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_SYMBOLP) {
      if (SYMBOLP(m_value)) {
        m_pc = OPERANDS;
        goto loop;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_TRUE_RET) {
      if (m_value != scm_false) goto pop_cont;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_FALSE_RET) {
      if (m_value == scm_false) goto pop_cont;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_TRUE_RET_CONST) {
      if (m_value != scm_false) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_FALSE_RET_CONST) {
      if (m_value == scm_false) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_EQP_RET_CONST) {
      m_sp--;
      assert(m_sp >= m_fp);
      if (m_sp[0] == m_value) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_PAIRP_RET_CONST) {
      if (PAIRP(m_value)) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_SYMBOLP_RET_CONST) {
      if (SYMBOLP(m_value)) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NOT_PAIRP_RET_CONST) {
      if (!PAIRP(m_value)) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NOT_NULLP_RET_CONST) {
      if (m_value != scm_nil) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NOT_EQP_RET_CONST) {
      m_sp--;
      assert(m_sp >= m_fp);
      if (m_sp[0] != m_value) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_IF_NOT_SYMBOLP_RET_CONST) {
      if (!SYMBOLP(m_value)) {
        m_value = OPERANDS;
        goto pop_cont;
      }
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_CLOSE) {
      if (STACKP(m_env)) {
        m_env = save_env(m_env);
        update_cont(m_cont);
      }
#if PREBIND_CLOSE
      m_value = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
#else
      scm_obj_t spec = CAR(OPERANDS);
      scm_obj_t code = CDR(OPERANDS);
      scm_obj_t doc = CDDR(spec);
      m_value = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
#endif
      m_pc = CDR(m_pc);
#if STDEBUG
      check_vm_state();
#endif
      goto loop;
    }

    CASE(VMOP_SET_GLOC) {
      scm_gloc_t gloc = (scm_gloc_t)CAR(OPERANDS);
      assert(GLOCP(gloc));
      m_heap->write_barrier(m_value);
      gloc->value = m_value;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_SET_ILOC) {
      scm_obj_t* slot = lookup_iloc(CAR(OPERANDS));
      if (!STACKP(slot)) {
        m_heap->write_barrier(m_value);
      }
      *slot = m_value;
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_PUSH_CONS) {
      m_sp[-1] = make_pair(m_heap, m_sp[-1], m_value);
      m_pc = CDR(m_pc);
      goto loop;
    }

    CASE(VMOP_RET_CONS) {
      m_value = make_pair(m_heap, m_sp[-1], m_value);
      goto pop_cont;
    }

    CASE(VMOP_RET_EQP) {
      m_value = (m_sp[-1] == m_value) ? scm_true : scm_false;
      goto pop_cont;
    }

    CASE(VMOP_RET_NULLP) {
      m_value = (m_value == scm_nil) ? scm_true : scm_false;
      goto pop_cont;
    }

    CASE(VMOP_RET_PAIRP) {
      m_value = PAIRP(m_value) ? scm_true : scm_false;
      goto pop_cont;
    }

    CASE(VMOP_RET_CLOSE) {
      if (STACKP(m_env)) {
        m_env = save_env(m_env);
        update_cont(m_cont);
      }
#if PREBIND_CLOSE
      m_value = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
#else
      scm_obj_t spec = CAR(OPERANDS);
      scm_obj_t code = CDR(OPERANDS);
      scm_obj_t doc = CDDR(spec);
      m_value = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
#endif
#if STDEBUG
      check_vm_state();
#endif
      goto pop_cont;
    }

    CASE(VMOP_PUSH_NADD_ILOC) {
      assert(FIXNUMP(CADR(OPERANDS)));
      if (m_sp < m_stack_limit) {
        obj = *lookup_iloc(CAR(OPERANDS));
        if (FIXNUMP(obj)) {
          intptr_t n = FIXNUM(obj) + FIXNUM(CADR(OPERANDS));
          if ((n <= FIXNUM_MAX) & (n >= FIXNUM_MIN)) {
            m_sp[0] = MAKEFIXNUM(n);
            m_sp++;
            m_pc = CDR(m_pc);
            goto loop;
          }
        }
        goto FALLBACK_PUSH_NADD_ILOC;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_CADR_ILOC) {
      if (m_sp < m_stack_limit) {
        obj = *lookup_iloc(CAR(OPERANDS));
        if (PAIRP(obj)) {
          obj = CDR(obj);
          if (PAIRP(obj)) {
            m_sp[0] = CAR(obj);
            m_sp++;
            m_pc = CDR(m_pc);
            goto loop;
          }
        }
        goto ERROR_PUSH_CADR_ILOC;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_PUSH_CDDR_ILOC) {
      if (m_sp < m_stack_limit) {
        obj = *lookup_iloc(CAR(OPERANDS));
        if (PAIRP(obj)) {
          obj = CDR(obj);
          if (PAIRP(obj)) {
            m_sp[0] = CDR(obj);
            m_sp++;
            m_pc = CDR(m_pc);
            goto loop;
          }
        }
        goto ERROR_PUSH_CDDR_ILOC;
      }
      goto COLLECT_STACK_ONE;
    }

    CASE(VMOP_CADR_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (PAIRP(obj)) {
        obj = CDR(obj);
        if (PAIRP(obj)) {
          m_value = CAR(obj);
          m_pc = CDR(m_pc);
          goto loop;
        }
      }
      goto ERROR_CADR_ILOC;
    }

    CASE(VMOP_CDDR_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (PAIRP(obj)) {
        obj = CDR(obj);
        if (PAIRP(obj)) {
          m_value = CDR(obj);
          m_pc = CDR(m_pc);
          goto loop;
        }
      }
      goto ERROR_CDDR_ILOC;
    }

    CASE(VMOP_EQ_N_ILOC) {
      assert(FIXNUMP(CADR(OPERANDS)));
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        m_value = ((intptr_t)obj == (intptr_t)CADR(OPERANDS)) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_EQ_N_ILOC;
    }

    CASE(VMOP_LT_N_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        m_value = ((intptr_t)CADR(OPERANDS) > (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_LT_N_ILOC;
    }

    CASE(VMOP_GE_N_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        m_value = ((intptr_t)CADR(OPERANDS) <= (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_GE_N_ILOC;
    }

    CASE(VMOP_LE_N_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        m_value = ((intptr_t)CADR(OPERANDS) >= (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_LE_N_ILOC;
    }

    CASE(VMOP_GT_N_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        m_value = ((intptr_t)CADR(OPERANDS) < (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_GT_N_ILOC;
    }

    CASE(VMOP_NADD_ILOC) {
      assert(FIXNUMP(CADR(OPERANDS)));
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(obj)) {
        intptr_t n = FIXNUM(obj) + FIXNUM(CADR(OPERANDS));
        if ((n <= FIXNUM_MAX) & (n >= FIXNUM_MIN)) {
          m_value = MAKEFIXNUM(n);
          m_pc = CDR(m_pc);
          goto loop;
        }
      }
      goto FALLBACK_NADD_ILOC;
    }

    CASE(VMOP_EQ_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(m_value) & FIXNUMP(obj)) {
        m_value = ((intptr_t)m_value == (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_EQ_ILOC;
    }

    CASE(VMOP_LT_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(m_value) & FIXNUMP(obj)) {
        m_value = ((intptr_t)m_value < (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_LT_ILOC;
    }

    CASE(VMOP_LE_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(m_value) & FIXNUMP(obj)) {
        m_value = ((intptr_t)m_value <= (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_LE_ILOC;
    }

    CASE(VMOP_GT_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(m_value) & FIXNUMP(obj)) {
        m_value = ((intptr_t)m_value > (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_GT_ILOC;
    }

    CASE(VMOP_GE_ILOC) {
      obj = *lookup_iloc(CAR(OPERANDS));
      if (FIXNUMP(m_value) & FIXNUMP(obj)) {
        m_value = ((intptr_t)m_value >= (intptr_t)obj) ? scm_true : scm_false;
        m_pc = CDR(m_pc);
        goto loop;
      }
      goto FALLBACK_GE_ILOC;
    }

    CASE(VMOP_TOUCH_GLOC) { goto THUNK_TOUCH_GLOC; }

    CASE(VMOP_SUBR_GLOC_OF) { goto THUNK_SUBR_GLOC_OF; }

    CASE(VMOP_PUSH_SUBR_GLOC_OF) { goto THUNK_PUSH_SUBR_GLOC_OF; }

    CASE(VMOP_RET_SUBR_GLOC_OF) { goto THUNK_RET_SUBR_GLOC_OF; }

    CASE(VMOP_VM_ESCAPE) { return; }

  }  // SWITCH()
  goto ERROR_BAD_INSTRUCTION;

APPLY_APPLY:
  if (m_sp - m_fp >= 2) {
    m_value = m_fp[0];
    m_fp++;
    obj = m_sp[-1];
    m_sp--;
    if (CLOSUREP(m_value)) {
      int x = apply_apply_closure(obj);
      if (x == apply_apply_trace_n_loop) {
        scm_closure_t closure = (scm_closure_t)m_value;
        if (closure->code) {
          intptr_t (*thunk)(intptr_t) = (intptr_t(*)(intptr_t))closure->code;
          intptr_t n = (*thunk)((intptr_t)this);
          NATIVE_THUNK_POST_DISPATCH(n);
        }
        goto trace_n_loop;
      }
      if (x == apply_apply_wrong_number_args) goto ERROR_APPLY_WRONG_NUMBER_ARGS;
      assert(x != apply_apply_pop_cont);
      goto ERROR_PROC_APPLY_BAD_LAST_ARGS;
    }
    if (SUBRP(m_value)) {
      int x = apply_apply_subr(obj);
      if (x == apply_apply_pop_cont) goto pop_cont;
      if (x == apply_apply_trace_n_loop) goto trace_n_loop;
      if (x == apply_apply_wrong_number_args) goto ERROR_APPLY_WRONG_NUMBER_ARGS;
      goto ERROR_PROC_APPLY_BAD_LAST_ARGS;
    }
    scm_obj_t lst = obj;
    while (PAIRP(lst)) {
      if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
      m_sp[0] = CAR(lst);
      m_sp++;
      lst = CDR(lst);
    }
    if (lst == scm_nil) goto APPLY_SPECIAL;
    goto ERROR_PROC_APPLY_BAD_LAST_ARGS;
  }
  goto ERROR_PROC_APPLY_WRONG_NUMBER_ARGS;

APPLY_VALUES:
  if (m_sp - m_fp == 2) {
    m_value = m_fp[0];
    scm_obj_t args = m_fp[1];
    if (VALUESP(args)) {
      scm_values_t values = (scm_values_t)args;
      intptr_t argc = HDR_VALUES_COUNT(values->hdr);
      m_sp = m_fp;
      if (m_sp + argc >= m_stack_limit) collect_stack(sizeof(scm_obj_t) * argc);
      for (intptr_t i = 0; i < argc; i++) m_sp[i] = values->elts[i];
      m_sp += argc;
      goto apply;
    } else {
      m_fp[0] = args;
      m_sp = m_fp + 1;
      goto apply;
    }
  }
  goto ERROR_APPLY_VALUES_WRONG_NUMBER_ARGS;

#if USE_FAST_DYNAMIC_WIND
APPLY_CONT : {
  scm_cont_t cont = (scm_cont_t)m_value;
  if (cont->wind_rec == scm_unspecified || cont->wind_rec == m_current_dynamic_wind_record) {
    intptr_t argc = m_sp - m_fp;
    m_cont = cont->cont;
    if (argc == 1) {
      m_value = *m_fp;
      goto pop_cont;
    }
    scm_values_t values = make_values(m_heap, argc);
    for (intptr_t i = 0; i < argc; i++) values->elts[i] = m_fp[i];
    m_value = values;
    goto pop_cont;
  } else {
    scm_obj_t lst = scm_nil;
    scm_obj_t* last = m_sp;
    while (--last >= m_fp) lst = make_pair(m_heap, last[0], lst);
    scm_obj_t proc = lookup_system_closure(".@perform-dynamic-wind");
    apply_scheme(proc, 3, cont->wind_rec, make_cont(m_heap, scm_unspecified, cont->cont), lst);
    m_sp = m_fp;
    m_pc = CDR(m_pc);
    goto loop;
  }
}
#else
APPLY_CONT : {
  intptr_t argc = m_sp - m_fp;
  scm_cont_t cont = (scm_cont_t)m_value;
  m_cont = cont->cont;
  if (argc == 1) {
    m_value = *m_fp;
    goto pop_cont;
  }
  scm_values_t values = make_values(m_heap, argc);
  for (intptr_t i = 0; i < argc; i++) values->elts[i] = m_fp[i];
  m_value = values;
  goto pop_cont;
}
#endif

APPLY_CALLCC:
  if (m_sp - m_fp == 1) {
    scm_obj_t proc = m_fp[0];
    m_cont = save_cont(m_cont);
    m_env = save_env(m_env);
    update_cont(m_cont);
    m_value = proc;
    m_fp = m_stack_top;
    m_sp = m_stack_top + 1;
    m_fp[0] = make_cont(m_heap, m_current_dynamic_wind_record, m_cont);
#if STDEBUG
    check_vm_state();
#endif
    goto apply;
  }
  goto ERROR_CALLCC_WRONG_NUMBER_ARGS;

APPLY_SPECIAL:
  if (m_value == scm_proc_apply) goto APPLY_APPLY;
  if (m_value == scm_proc_apply_values) goto APPLY_VALUES;
  if (m_value == scm_proc_callcc) goto APPLY_CALLCC;
  if (CONTP(m_value)) goto APPLY_CONT;
  goto ERROR_INVALID_APPLICATION;

APPLY_VARIADIC : {
  scm_closure_t closure = (scm_closure_t)m_value;
  intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
  int rest = 0;
  if (args < 0) {
    args = -args - 1;
    rest = 1;
  }
  intptr_t argc = m_sp - m_fp;
  if (rest & (argc >= args)) {
    scm_obj_t opt = scm_nil;
    scm_obj_t* first = m_sp - argc + args;
    scm_obj_t* last = m_sp;
    while (--last >= first) opt = make_pair(m_heap, *last, opt);
    *first = opt;
    m_sp = first + 1;
    args = args + 1;
    vm_env_t env = (vm_env_t)m_sp;
    env->count = args;
    env->up = closure->env;
    m_sp = m_fp = (scm_obj_t*)(env + 1);
    m_pc = closure->pc;
    m_env = &env->up;
    if (closure->code) {
      intptr_t (*thunk)(intptr_t) = (intptr_t(*)(intptr_t))closure->code;
      intptr_t n = (*thunk)((intptr_t)this);
      NATIVE_THUNK_POST_DISPATCH(n);
    }
    goto trace_n_loop;
  }
  goto ERROR_APPLY_WRONG_NUMBER_ARGS;
}

COLLECT_STACK_ONE:
  collect_stack(sizeof(scm_obj_t));
  goto loop;

COLLECT_STACK_CONT_REC:
  collect_stack(sizeof(vm_cont_rec_t));
  goto loop;

COLLECT_STACK_ENV_REC:
  collect_stack(sizeof(vm_env_rec_t));
  goto loop;

COLLECT_STACK_ENV_REC_N_ONE:
  collect_stack(sizeof(vm_env_rec_t) + sizeof(scm_obj_t));
  goto loop;

COLLECT_STACK_ENV_REC_N_APPLY:
  collect_stack(sizeof(vm_env_rec_t));
  goto apply;

COLLECT_STACK_ENV_REC_N_OPERAND:
  collect_stack(sizeof(vm_env_rec_t) + sizeof(scm_obj_t*) * FIXNUM(OPERANDS));
  goto loop;

FALLBACK_PUSH_NADD_ILOC:
  if (number_pred(obj)) {
    m_sp[0] = arith_add(m_heap, obj, CADR(OPERANDS));
    m_sp++;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_PUSH_NADD_ILOC;

FALLBACK_NADD_ILOC:
  if (number_pred(obj)) {
    m_value = arith_add(m_heap, obj, CADR(OPERANDS));
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_NADD_ILOC;

FALLBACK_EQ_N_ILOC:
  if (number_pred(obj)) {
    m_value = n_equal_pred(m_heap, obj, CADR(OPERANDS)) ? scm_true : scm_false;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_EQ_N_ILOC;

FALLBACK_LT_N_ILOC:
  if (real_pred(obj)) {
    m_value = n_compare(m_heap, obj, CADR(OPERANDS)) < 0 ? scm_true : scm_false;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_LT_N_ILOC;

FALLBACK_LE_N_ILOC:
  if (real_pred(obj)) {
    m_value = n_compare(m_heap, obj, CADR(OPERANDS)) <= 0 ? scm_true : scm_false;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_LE_N_ILOC;

FALLBACK_GT_N_ILOC:
  if (real_pred(obj)) {
    m_value = n_compare(m_heap, obj, CADR(OPERANDS)) > 0 ? scm_true : scm_false;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_GT_N_ILOC;

FALLBACK_GE_N_ILOC:
  if (real_pred(obj)) {
    m_value = n_compare(m_heap, obj, CADR(OPERANDS)) >= 0 ? scm_true : scm_false;
    m_pc = CDR(m_pc);
    goto loop;
  }
  goto ERROR_GE_N_ILOC;

FALLBACK_EQ_ILOC : {
  int bad;
  if (number_pred(m_value)) {
    if (number_pred(obj)) {
      m_value = n_equal_pred(m_heap, m_value, obj) ? scm_true : scm_false;
      m_pc = CDR(m_pc);
      goto loop;
    }
    if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
    bad = 1;
  } else {
    bad = 0;
  }
  scm_obj_t argv[2] = {m_value, obj};
  wrong_type_argument_violation(this, "=", bad, "number", argv[bad], 2, argv);
  goto RESUME_LOOP;
}

FALLBACK_LT_ILOC : {
  int bad;
  if (real_pred(m_value)) {
    if (real_pred(obj)) {
      m_value = (n_compare(m_heap, m_value, obj) < 0) ? scm_true : scm_false;
      m_pc = CDR(m_pc);
      goto loop;
    }
    if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
    bad = 1;
  } else {
    bad = 0;
  }
  scm_obj_t argv[2] = {m_value, obj};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
  goto RESUME_LOOP;
}

FALLBACK_LE_ILOC : {
  int bad;
  if (real_pred(m_value)) {
    if (real_pred(obj)) {
      m_value = (n_compare(m_heap, m_value, obj) <= 0) ? scm_true : scm_false;
      m_pc = CDR(m_pc);
      goto loop;
    }
    if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
    bad = 1;
  } else {
    bad = 0;
  }
  scm_obj_t argv[2] = {m_value, obj};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
  goto RESUME_LOOP;
}

FALLBACK_GT_ILOC : {
  int bad;
  if (real_pred(m_value)) {
    if (real_pred(obj)) {
      m_value = (n_compare(m_heap, m_value, obj) > 0) ? scm_true : scm_false;
      m_pc = CDR(m_pc);
      goto loop;
    }
    if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
    bad = 1;
  } else {
    bad = 0;
  }
  scm_obj_t argv[2] = {m_value, obj};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
  goto RESUME_LOOP;
}

FALLBACK_GE_ILOC : {
  int bad;
  if (real_pred(m_value)) {
    if (real_pred(obj)) {
      m_value = (n_compare(m_heap, m_value, obj) >= 0) ? scm_true : scm_false;
      m_pc = CDR(m_pc);
      goto loop;
    }
    if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
    bad = 1;
  } else {
    bad = 0;
  }
  scm_obj_t argv[2] = {m_value, obj};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
  goto RESUME_LOOP;
}

THUNK_TOUCH_GLOC : {
  assert(GLOCP(OPERANDS));
  if (((scm_gloc_t)OPERANDS)->value != scm_undef) {
    m_heap->write_barrier(CADR(m_pc));
    m_heap->write_barrier(CDDR(m_pc));
    CAR(m_pc) = CADR(m_pc);
    CDR(m_pc) = CDDR(m_pc);
    goto loop;
  }
  goto ERROR_TOUCH_GLOC;
}

THUNK_SUBR_GLOC_OF : {
  assert(GLOCP(CAR(OPERANDS)));
  scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
  if (SUBRP(subr)) {
    m_heap->write_barrier(subr);
    CAAR(m_pc) = opcode_to_instruction(VMOP_SUBR);
    CAR(OPERANDS) = subr;
    goto loop;
  }
  system_error("system error: inconsistent code in auto compile cache");
}

THUNK_PUSH_SUBR_GLOC_OF : {
  assert(GLOCP(CAR(OPERANDS)));
  scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
  if (SUBRP(subr)) {
    m_heap->write_barrier(subr);
    CAAR(m_pc) = opcode_to_instruction(VMOP_PUSH_SUBR);
    CAR(OPERANDS) = subr;
    goto loop;
  }
  system_error("system error: inconsistent code in auto compile cache");
}

THUNK_RET_SUBR_GLOC_OF : {
  assert(GLOCP(CAR(OPERANDS)));
  scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
  if (SUBRP(subr)) {
    m_heap->write_barrier(subr);
    CAAR(m_pc) = opcode_to_instruction(VMOP_RET_SUBR);
    CAR(OPERANDS) = subr;
    goto loop;
  }
  system_error("system error: inconsistent code in auto compile cache");
}

ERROR_NADD_ILOC:
ERROR_PUSH_NADD_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "operator(+ -)", 0, "number", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_EQ_N_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "=", 0, "number", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_LT_N_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_LE_N_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_GT_N_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_GE_N_ILOC : {
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  scm_obj_t argv[2] = {obj, CADR(OPERANDS)};
  wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_PUSH_CAR_ILOC:
ERROR_CAR_ILOC:
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  wrong_type_argument_violation(this, "car", 0, "pair", obj, 1, &obj);
  goto RESUME_LOOP;
ERROR_PUSH_CDR_ILOC:
ERROR_CDR_ILOC:
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  wrong_type_argument_violation(this, "cdr", 0, "pair", obj, 1, &obj);
  goto RESUME_LOOP;
ERROR_PUSH_CADR_ILOC:
ERROR_CADR_ILOC:
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  wrong_type_argument_violation(this, "cadr", 0, "appropriate list structure", obj, 1, &obj);
  goto RESUME_LOOP;
ERROR_PUSH_CDDR_ILOC:
ERROR_CDDR_ILOC:
  if (obj == scm_undef) goto ERROR_LETREC_VIOLATION;
  wrong_type_argument_violation(this, "cddr", 0, "appropriate list structure", obj, 1, &obj);
  goto RESUME_LOOP;

ERROR_GLOC:
ERROR_RET_GLOC:
ERROR_PUSH_GLOC:
ERROR_TOUCH_GLOC:
  undefined_violation(this, ((scm_gloc_t)OPERANDS)->variable, NULL);
  goto RESUME_LOOP;

ERROR_RET_ILOC:
ERROR_APPLY_ILOC:
  letrec_violation(this);
  goto RESUME_LOOP;

ERROR_PUSH_VECTREF_ILOC : {
  scm_obj_t argv[2] = {obj, m_sp[-1]};
  if (VECTORP(argv[0])) {
    if (exact_non_negative_integer_pred(argv[1])) {
      invalid_argument_violation(this, "vector-ref", "index out of bounds,", argv[1], 1, 2, argv);
    } else {
      wrong_type_argument_violation(this, "vector-ref", 1, "exact non-negative integer", argv[1], 2, argv);
    }
    goto RESUME_LOOP;
  }
  wrong_type_argument_violation(this, "vector-ref", 0, "vector", argv[0], 2, argv);
  goto RESUME_LOOP;
}
ERROR_VECTREF_ILOC : {
  scm_obj_t argv[2] = {obj, m_value};
  if (VECTORP(argv[0])) {
    if (exact_non_negative_integer_pred(argv[1])) {
      invalid_argument_violation(this, "vector-ref", "index out of bounds,", argv[1], 1, 2, argv);
    } else {
      wrong_type_argument_violation(this, "vector-ref", 1, "exact non-negative integer", argv[1], 2, argv);
    }
    goto RESUME_LOOP;
  }
  wrong_type_argument_violation(this, "vector-ref", 0, "vector", argv[0], 2, argv);
  goto RESUME_LOOP;
}

ERROR_LETREC_VIOLATION:
  letrec_violation(this);
  goto RESUME_LOOP;

ERROR_APPLY_GLOC:
  undefined_violation(this, ((scm_gloc_t)CAR(OPERANDS))->variable, NULL);
  goto BACK_TO_TRACE_N_LOOP;

ERROR_APPLY_WRONG_NUMBER_ARGS : {
  scm_closure_t closure = (scm_closure_t)m_value;
  intptr_t args = HDR_CLOSURE_ARGS(closure->hdr);
  int rest = 0;
  if (args < 0) {
    args = -args - 1;
    rest = 1;
  }
  if (rest)
    wrong_number_of_arguments_violation(this, m_value, args, -1, m_sp - m_fp, m_fp);
  else
    wrong_number_of_arguments_violation(this, m_value, args, args, m_sp - m_fp, m_fp);
  assert(CAR(m_pc) == scm_unspecified);
  goto BACK_TO_TRACE_N_LOOP;
}

ERROR_PROC_APPLY_WRONG_NUMBER_ARGS:
  wrong_number_of_arguments_violation(this, "apply", 2, -1, m_sp - m_fp, m_fp);
  assert(CAR(m_pc) == scm_unspecified);
  goto BACK_TO_TRACE_N_LOOP;

ERROR_PROC_APPLY_BAD_LAST_ARGS:
  wrong_type_argument_violation(this, "apply", -1, "proper list for last argument", obj, -1, NULL);
  assert(CAR(m_pc) == scm_unspecified);
  goto BACK_TO_TRACE_N_LOOP;

ERROR_APPLY_VALUES_WRONG_NUMBER_ARGS:
  wrong_number_of_arguments_violation(this, "apply-values", 2, 2, m_sp - m_fp, m_fp);
  assert(CAR(m_pc) == scm_unspecified);
  goto BACK_TO_TRACE_N_LOOP;

ERROR_CALLCC_WRONG_NUMBER_ARGS:
  wrong_number_of_arguments_violation(this, "call-with-current-continuation", 1, 1, m_sp - m_fp, m_fp);
  assert(CAR(m_pc) == scm_unspecified);
  goto BACK_TO_TRACE_N_LOOP;

ERROR_INVALID_APPLICATION:
  invalid_application_violation(this, m_value, m_sp - m_fp, m_fp);
  goto BACK_TO_TRACE_N_LOOP;

RESUME_LOOP:
  m_sp = m_fp;
  m_pc = CDR(m_pc);
  goto loop;

BACK_TO_TRACE_N_LOOP:
  m_sp = m_fp;
  m_pc = CDR(m_pc);
  goto trace_n_loop;

ERROR_BAD_INSTRUCTION:
  system_error("system error: invalid vm instruction %d", instruction_to_opcode(CAAR(m_pc)));
  return;
}
