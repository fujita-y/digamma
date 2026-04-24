// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef PORT_H_INCLUDED
#define PORT_H_INCLUDED

#include "core.h"
#include "object.h"

void port_finalize(scm_port_rec_t* rec);
std::ostream* port_get_ostream(scm_obj_t port);
std::istream* port_get_istream(scm_obj_t port);
scm_obj_t port_get_bytes(scm_obj_t port, int bsize);
scm_obj_t port_get_bytes_async(scm_obj_t port, scm_obj_t bv);
scm_obj_t port_open_input_file(const char* filename);
scm_obj_t port_open_output_file(const char* filename);
scm_obj_t port_put_byte(scm_obj_t port, uint8_t byte);
scm_obj_t port_put_bytes(scm_obj_t port, const uint8_t* byte, int bsize);
scm_obj_t port_put_char(scm_obj_t port, scm_obj_t ch);
scm_obj_t port_put_string(scm_obj_t port, scm_obj_t str);
scm_obj_t port_flush_output(scm_obj_t port);
void port_close(scm_obj_t port);
scm_obj_t port_open_string_output_port();
scm_obj_t port_get_output_string(scm_obj_t port);
scm_obj_t port_standard_input();
scm_obj_t port_standard_output();
scm_obj_t port_standard_error();
bool fd_nonblock_byte_ready(int fd);
bool port_nonblock_byte_ready(scm_obj_t port);
scm_obj_t port_put_bytes_async(scm_obj_t port, const uint8_t* byte, int bsize);
scm_obj_t port_put_string_async(scm_obj_t port, scm_obj_t str);
scm_obj_t port_get_bytes_n(scm_obj_t port, int n);
scm_obj_t port_get_bytes_n_async(scm_obj_t port, int n);

inline bool is_input_port(scm_obj_t port) { return port_get_istream(port) != nullptr; }
inline bool is_output_port(scm_obj_t port) { return port_get_ostream(port) != nullptr; }

#endif
