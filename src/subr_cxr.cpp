// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "object.h"
#include "context.h"
#include "subr.h"

// caar, cadr, etc. up to 5 levels - R6RS / SRFI 1 extensions

static inline scm_obj_t _car(scm_obj_t self, scm_obj_t a1) {
  if (a1 == (scm_obj_t) nullptr) [[unlikely]] {
    return (scm_obj_t) nullptr;
  }
  if (!is_cons(a1)) [[unlikely]] {
    return (scm_obj_t) nullptr;
  }
  scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
  return cons->car;
}

static inline scm_obj_t _cdr(scm_obj_t self, scm_obj_t a1) {
  if (a1 == (scm_obj_t) nullptr) [[unlikely]] {
    return (scm_obj_t) nullptr;
  }
  if (!is_cons(a1)) [[unlikely]] {
    return (scm_obj_t) nullptr;
  }
  scm_cons_rec_t* cons = (scm_cons_rec_t*)a1;
  return cons->cdr;
}

SUBR subr_caar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, a1));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, a1));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, a1));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, a1));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, a1)));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _car(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _cdr(self, a1))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _car(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caadaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caadaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caadadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caadadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caaddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caaddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caadddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _car(self, _cdr(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caadddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _car(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caddaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caddaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caddadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caddadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cadddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cadddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_caddddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _car(self, _cdr(self, _cdr(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("caddddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _car(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdadaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdadaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdadadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdadadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdaddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdaddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdadddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _car(self, _cdr(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdadddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddaaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddaaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddaadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddaadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddadar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddadar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddaddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _car(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddaddr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdddaar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _car(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdddaar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdddadr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _car(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdddadr: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cddddar(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _cdr(self, _car(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cddddar: expected appropriate list structure");
  }
  return result;
}
SUBR subr_cdddddr(scm_obj_t self, scm_obj_t a1) {
  scm_obj_t result = _cdr(self, _cdr(self, _cdr(self, _cdr(self, _cdr(self, a1)))));
  if (result == (scm_obj_t) nullptr) [[unlikely]] {
    throw std::runtime_error("cdddddr: expected appropriate list structure");
  }
  return result;
}

void init_subr_cxr() {
  auto reg = [](const char* name, void* func, int req, int opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt, 0, nullptr, 1));
  };

  reg("caar", (void*)subr_caar, 1, 0);
  reg("cadr", (void*)subr_cadr, 1, 0);
  reg("cdar", (void*)subr_cdar, 1, 0);
  reg("cddr", (void*)subr_cddr, 1, 0);
  reg("caaar", (void*)subr_caaar, 1, 0);
  reg("caadr", (void*)subr_caadr, 1, 0);
  reg("cadar", (void*)subr_cadar, 1, 0);
  reg("caddr", (void*)subr_caddr, 1, 0);
  reg("cdaar", (void*)subr_cdaar, 1, 0);
  reg("cdadr", (void*)subr_cdadr, 1, 0);
  reg("cddar", (void*)subr_cddar, 1, 0);
  reg("cdddr", (void*)subr_cdddr, 1, 0);
  reg("caaaar", (void*)subr_caaaar, 1, 0);
  reg("caaadr", (void*)subr_caaadr, 1, 0);
  reg("caadar", (void*)subr_caadar, 1, 0);
  reg("caaddr", (void*)subr_caaddr, 1, 0);
  reg("cadaar", (void*)subr_cadaar, 1, 0);
  reg("cadadr", (void*)subr_cadadr, 1, 0);
  reg("caddar", (void*)subr_caddar, 1, 0);
  reg("cadddr", (void*)subr_cadddr, 1, 0);
  reg("cdaaar", (void*)subr_cdaaar, 1, 0);
  reg("cdaadr", (void*)subr_cdaadr, 1, 0);
  reg("cdadar", (void*)subr_cdadar, 1, 0);
  reg("cdaddr", (void*)subr_cdaddr, 1, 0);
  reg("cddaar", (void*)subr_cddaar, 1, 0);
  reg("cddadr", (void*)subr_cddadr, 1, 0);
  reg("cdddar", (void*)subr_cdddar, 1, 0);
  reg("cddddr", (void*)subr_cddddr, 1, 0);
  reg("caaaaar", (void*)subr_caaaaar, 1, 0);
  reg("caaaadr", (void*)subr_caaaadr, 1, 0);
  reg("caaadar", (void*)subr_caaadar, 1, 0);
  reg("caaaddr", (void*)subr_caaaddr, 1, 0);
  reg("caadaar", (void*)subr_caadaar, 1, 0);
  reg("caadadr", (void*)subr_caadadr, 1, 0);
  reg("caaddar", (void*)subr_caaddar, 1, 0);
  reg("caadddr", (void*)subr_caadddr, 1, 0);
  reg("cadaaar", (void*)subr_cadaaar, 1, 0);
  reg("cadaadr", (void*)subr_cadaadr, 1, 0);
  reg("cadadar", (void*)subr_cadadar, 1, 0);
  reg("cadaddr", (void*)subr_cadaddr, 1, 0);
  reg("caddaar", (void*)subr_caddaar, 1, 0);
  reg("caddadr", (void*)subr_caddadr, 1, 0);
  reg("cadddar", (void*)subr_cadddar, 1, 0);
  reg("caddddr", (void*)subr_caddddr, 1, 0);
  reg("cdaaaar", (void*)subr_cdaaaar, 1, 0);
  reg("cdaaadr", (void*)subr_cdaaadr, 1, 0);
  reg("cdaadar", (void*)subr_cdaadar, 1, 0);
  reg("cdaaddr", (void*)subr_cdaaddr, 1, 0);
  reg("cdadaar", (void*)subr_cdadaar, 1, 0);
  reg("cdadadr", (void*)subr_cdadadr, 1, 0);
  reg("cdaddar", (void*)subr_cdaddar, 1, 0);
  reg("cdadddr", (void*)subr_cdadddr, 1, 0);
  reg("cddaaar", (void*)subr_cddaaar, 1, 0);
  reg("cddaadr", (void*)subr_cddaadr, 1, 0);
  reg("cddadar", (void*)subr_cddadar, 1, 0);
  reg("cddaddr", (void*)subr_cddaddr, 1, 0);
  reg("cdddaar", (void*)subr_cdddaar, 1, 0);
  reg("cdddadr", (void*)subr_cdddadr, 1, 0);
  reg("cddddar", (void*)subr_cddddar, 1, 0);
  reg("cdddddr", (void*)subr_cdddddr, 1, 0);
}
