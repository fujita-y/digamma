// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"
#include "heap.h"
#include "port.h"
#include "ioerror.h"
#include "arith.h"

void
raise_io_codec_error(VM* vm, const char* who, int operation, const char* message, scm_obj_t io, scm_obj_t ch)
{
    vm->backtrace_seek();
    scm_symbol_t arg1 = make_symbol(vm->m_heap, who);
    scm_string_t arg2 = make_string(vm->m_heap, message);

    switch (operation) {
        case SCM_PORT_OPERATION_ENCODE: {
            vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-encoding-error"), 4, arg1, arg2, io, ch);
        } break;

        case SCM_PORT_OPERATION_DECODE: {
            vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-decoding-error"), 3, arg1, arg2, io);
        } break;

        default: fatal("%s:%u wrong port operation code", __FILE__, __LINE__);

    }
}

void
raise_io_error(VM* vm, const char* who, int operation, const char* message, int err, scm_obj_t io, scm_obj_t filename)
{
    vm->backtrace_seek();
    scm_symbol_t arg1 = make_symbol(vm->m_heap, who);
    scm_string_t arg2;
    char buf[256];
    if (err) {
        snprintf(buf, sizeof(buf), "%s (%d)", message, err);
        arg2 = make_string(vm->m_heap, buf);
    } else {
        arg2 = make_string(vm->m_heap, message);
    }
    switch (operation) {
        case SCM_PORT_OPERATION_OPEN: {
            switch (err) {
                case ENOENT:
                    vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-does-not-exist-error"), 3, arg1, arg2, filename);
                    break;
                case EEXIST:
                    vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-already-exists-error"), 3, arg1, arg2, filename);
                    break;
                case EROFS: case EISDIR: case ETXTBSY:
                    vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-is-read-only-error"), 3, arg1, arg2, filename);
                    break;
                case EACCES:
                    vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-protection-error"), 3, arg1, arg2, filename);
                    break;
                default:
                    vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-error"), 3, arg1, arg2, filename);
                    break;
            }
        } break;

        case SCM_PORT_OPERATION_READ: {
             vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-read-error"), 3, arg1, arg2, io);
        } break;

        case SCM_PORT_OPERATION_WRITE: {
             vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-write-error"), 3, arg1, arg2, io);
        } break;

        case SCM_PORT_OPERATION_SEEK: {
             scm_obj_t pos = int64_to_integer(vm->m_heap, ((scm_port_t)io)->mark);
             vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-invalid-position-error"), 4, arg1, arg2, io, pos);
        } break;

        case SCM_SOCKET_OPERATION_OPEN: {
            vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-error"), 2, arg1, arg2);
        } break;

        case SCM_PORT_OPERATION_CLOSE:
        case SCM_PORT_OPERATION_STAT:
        case SCM_PORT_OPERATION_SELECT: {
            vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-error"), 3, arg1, arg2, io);
        } break;

        case SCM_SOCKET_OPERATION_READ:
        case SCM_SOCKET_OPERATION_WRITE:
        case SCM_SOCKET_OPERATION_ACCEPT: {
            vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-error"), 3, arg1, arg2, io);
            break;
        }

        default: fatal("%s:%u wrong port operation code", __FILE__, __LINE__);

    }
}

void
raise_io_filesystem_error(VM* vm, const char* who, const char* message, int err, scm_obj_t old_filename, scm_obj_t new_filename)
{
    vm->backtrace_seek();
    scm_symbol_t arg1 = make_symbol(vm->m_heap, who);
    scm_string_t arg2;
    char buf[256];
    if (err) {
        snprintf(buf, sizeof(buf), "%s (%d)", message, err);
        arg2 = make_string(vm->m_heap, buf);
    } else {
        arg2 = make_string(vm->m_heap, message);
    }
    switch (err) {
    case ENOENT:
    case EISDIR:
        vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-does-not-exist-error"), 3, arg1, arg2, old_filename);
        break;
    case EEXIST:
        vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-file-already-exists-error"), 3, arg1, arg2, new_filename);
        break;
    default:
        vm->apply_scheme(vm->lookup_system_closure(".@raise-i/o-error"), 4, arg1, arg2, old_filename, new_filename);
        break;
    }
}
