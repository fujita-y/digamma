// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef INHERENT_H_INCLUDED
#define INHERENT_H_INCLUDED

#include "core.h"

enum {
  VMOP_RET_GLOC,
  VMOP_RET_CLOSE,
  VMOP_RET_CONST,
  VMOP_RET_ILOC,
  VMOP_EXTEND_ENCLOSE,
  VMOP_EXTEND_ENCLOSE_LOCAL,
  VMOP_EXTEND_UNBOUND,
  VMOP_PUSH_CLOSE,
  VMOP_PUSH_CLOSE_LOCAL,
  VMOP_ENCLOSE,
  VMOP_PUSH_CADR_ILOC,
  VMOP_PUSH_CDDR_ILOC,
  VMOP_PUSH_CONS,
  VMOP_PUSH_GLOC,
  VMOP_PUSH_SUBR,
  VMOP_PUSH_NADD_ILOC,
  VMOP_NADD_ILOC,
  VMOP_PUSH_CAR_ILOC,
  VMOP_PUSH_CDR_ILOC,
  VMOP_PUSH_ILOC,
  VMOP_PUSH,
  VMOP_PUSH_CONST,
  VMOP_PUSH_ILOC1,
  VMOP_PUSH_ILOC0,
  VMOP_APPLY_ILOC_LOCAL,
  VMOP_APPLY_GLOC,
  VMOP_RET_SUBR,
  VMOP_APPLY_ILOC,
  VMOP_EXTEND,
  VMOP_CALL,
  VMOP_GLOC,
  VMOP_ILOC,
  VMOP_EQ_N_ILOC,
  VMOP_LT_N_ILOC,
  VMOP_LE_N_ILOC,
  VMOP_GE_N_ILOC,
  VMOP_GT_N_ILOC,
  VMOP_CADR_ILOC,
  VMOP_CDDR_ILOC,
  VMOP_CAR_ILOC,
  VMOP_CDR_ILOC,
  VMOP_CONST,
  VMOP_SUBR,
  VMOP_ILOC1,
  VMOP_ILOC0,
  VMOP_IF_TRUE,
  VMOP_IF_NULLP_RET_CONST,
  VMOP_IF_EQP,
  VMOP_IF_NULLP,
  VMOP_IF_PAIRP,
  VMOP_IF_SYMBOLP,
  VMOP_IF_TRUE_RET,
  VMOP_IF_FALSE_RET,
  VMOP_IF_TRUE_RET_CONST,
  VMOP_IF_FALSE_RET_CONST,
  VMOP_IF_EQP_RET_CONST,
  VMOP_IF_PAIRP_RET_CONST,
  VMOP_IF_SYMBOLP_RET_CONST,
  VMOP_IF_NOT_PAIRP_RET_CONST,
  VMOP_IF_NOT_NULLP_RET_CONST,
  VMOP_IF_NOT_EQP_RET_CONST,
  VMOP_IF_NOT_SYMBOLP_RET_CONST,
  VMOP_RET_CONS,
  VMOP_RET_EQP,
  VMOP_RET_NULLP,
  VMOP_RET_PAIRP,
  VMOP_APPLY,
  VMOP_CLOSE,
  VMOP_SET_GLOC,
  VMOP_SET_ILOC,
  VMOP_IF_FALSE_CALL,
  VMOP_TOUCH_GLOC,
  VMOP_VM_ESCAPE,
  VMOP_EQ_ILOC,
  VMOP_LT_ILOC,
  VMOP_LE_ILOC,
  VMOP_GT_ILOC,
  VMOP_GE_ILOC,
  VMOP_TOUCH_GLOC_OF,
  VMOP_SUBR_GLOC_OF,
  VMOP_RET_SUBR_GLOC_OF,
  VMOP_PUSH_SUBR_GLOC_OF,

  VMOP_INSTRUCTION_COUNT,

  VMOP_GLOC_OF = VMOP_INSTRUCTION_COUNT,
  VMOP_RET_GLOC_OF,
  VMOP_PUSH_GLOC_OF,
  VMOP_SET_GLOC_OF,
  VMOP_APPLY_GLOC_OF,
  VMOP_PUSH_CONST_UNSPEC,
  VMOP_CONST_UNSPEC,
  VMOP_RET_CONST_UNSPEC,
  VMOP_PUSH_CONST_UNDEF,
  VMOP_CONST_UNDEF,
  VMOP_RET_CONST_UNDEF,

  VMOP_MNEMNIC_COUNT,

  S_CODE_LITTLE = VMOP_MNEMNIC_COUNT,
  S_CODE_BIG,
  S_CODE_QUOTE,
  S_CODE_QUASIQUOTE,
  S_CODE_UNQUOTE,
  S_CODE_UNQUOTE_SPLICING,
  S_CODE_SYNTAX,
  S_CODE_QUASISYNTAX,
  S_CODE_UNSYNTAX,
  S_CODE_UNSYNTAX_SPLICING,
  S_CODE_LPAREN,
  S_CODE_RPAREN,
  S_CODE_LBRACK,
  S_CODE_RBRACK,
  S_CODE_DOT,

  NIL_STRING,
  NIL_VECTOR,
  NIL_BVECTOR,
  NIL_TUPLE,

#if USE_FLONUM_CONST
  FL_POSITIVE_ZERO,
  FL_NEGATIVE_ZERO,
  FL_NAN,
#endif

  INHERENT_TOTAL_COUNT
};

#endif
