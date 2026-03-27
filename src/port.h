// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef PORT_H_INCLUDED
#define PORT_H_INCLUDED

#include "core.h"
#include "object.h"

void port_finalize(scm_port_rec_t* rec);
scm_obj_t port_put_byte(scm_obj_t port, uint8_t byte);
scm_obj_t port_put_bytes(scm_obj_t port, const uint8_t* byte, int bsize);
scm_obj_t port_standard_input();
scm_obj_t port_standard_output();
scm_obj_t port_standard_error();

#endif
