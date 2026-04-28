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
  auto reg = [](const char* name, void* func, int req, bool opt) {
    context::environment_variable_set(make_symbol(name), make_closure(func, req, opt ? 1 : 0, 0, nullptr, 1));
  };

  reg("caar", (void*)subr_caar, 1, false);
  reg("cadr", (void*)subr_cadr, 1, false);
  reg("cdar", (void*)subr_cdar, 1, false);
  reg("cddr", (void*)subr_cddr, 1, false);
  reg("caaar", (void*)subr_caaar, 1, false);
  reg("caadr", (void*)subr_caadr, 1, false);
  reg("cadar", (void*)subr_cadar, 1, false);
  reg("caddr", (void*)subr_caddr, 1, false);
  reg("cdaar", (void*)subr_cdaar, 1, false);
  reg("cdadr", (void*)subr_cdadr, 1, false);
  reg("cddar", (void*)subr_cddar, 1, false);
  reg("cdddr", (void*)subr_cdddr, 1, false);
  reg("caaaar", (void*)subr_caaaar, 1, false);
  reg("caaadr", (void*)subr_caaadr, 1, false);
  reg("caadar", (void*)subr_caadar, 1, false);
  reg("caaddr", (void*)subr_caaddr, 1, false);
  reg("cadaar", (void*)subr_cadaar, 1, false);
  reg("cadadr", (void*)subr_cadadr, 1, false);
  reg("caddar", (void*)subr_caddar, 1, false);
  reg("cadddr", (void*)subr_cadddr, 1, false);
  reg("cdaaar", (void*)subr_cdaaar, 1, false);
  reg("cdaadr", (void*)subr_cdaadr, 1, false);
  reg("cdadar", (void*)subr_cdadar, 1, false);
  reg("cdaddr", (void*)subr_cdaddr, 1, false);
  reg("cddaar", (void*)subr_cddaar, 1, false);
  reg("cddadr", (void*)subr_cddadr, 1, false);
  reg("cdddar", (void*)subr_cdddar, 1, false);
  reg("cddddr", (void*)subr_cddddr, 1, false);
  reg("caaaaar", (void*)subr_caaaaar, 1, false);
  reg("caaaadr", (void*)subr_caaaadr, 1, false);
  reg("caaadar", (void*)subr_caaadar, 1, false);
  reg("caaaddr", (void*)subr_caaaddr, 1, false);
  reg("caadaar", (void*)subr_caadaar, 1, false);
  reg("caadadr", (void*)subr_caadadr, 1, false);
  reg("caaddar", (void*)subr_caaddar, 1, false);
  reg("caadddr", (void*)subr_caadddr, 1, false);
  reg("cadaaar", (void*)subr_cadaaar, 1, false);
  reg("cadaadr", (void*)subr_cadaadr, 1, false);
  reg("cadadar", (void*)subr_cadadar, 1, false);
  reg("cadaddr", (void*)subr_cadaddr, 1, false);
  reg("caddaar", (void*)subr_caddaar, 1, false);
  reg("caddadr", (void*)subr_caddadr, 1, false);
  reg("cadddar", (void*)subr_cadddar, 1, false);
  reg("caddddr", (void*)subr_caddddr, 1, false);
  reg("cdaaaar", (void*)subr_cdaaaar, 1, false);
  reg("cdaaadr", (void*)subr_cdaaadr, 1, false);
  reg("cdaadar", (void*)subr_cdaadar, 1, false);
  reg("cdaaddr", (void*)subr_cdaaddr, 1, false);
  reg("cdadaar", (void*)subr_cdadaar, 1, false);
  reg("cdadadr", (void*)subr_cdadadr, 1, false);
  reg("cdaddar", (void*)subr_cdaddar, 1, false);
  reg("cdadddr", (void*)subr_cdadddr, 1, false);
  reg("cddaaar", (void*)subr_cddaaar, 1, false);
  reg("cddaadr", (void*)subr_cddaadr, 1, false);
  reg("cddadar", (void*)subr_cddadar, 1, false);
  reg("cddaddr", (void*)subr_cddaddr, 1, false);
  reg("cdddaar", (void*)subr_cdddaar, 1, false);
  reg("cdddadr", (void*)subr_cdddadr, 1, false);
  reg("cddddar", (void*)subr_cddddar, 1, false);
  reg("cdddddr", (void*)subr_cdddddr, 1, false);
}
