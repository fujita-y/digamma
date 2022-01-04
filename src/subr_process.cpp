// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"
#include "violation.h"
#include "port.h"
#include "list.h"
#include "arith.h"

#include <cstring>

static scm_bvector_t make_posix_env(VM* vm, scm_obj_t env)
{
    scm_obj_t lst = env;
    if (listp(lst)) {
        int count = list_length(lst);
        int bsize = (count + 1) * sizeof(void*);
        while (PAIRP(lst)) {
            if (PAIRP(CAR(lst)) && STRINGP(CAAR(lst)) && STRINGP(CDAR(lst))) {
                scm_string_t lhs = (scm_string_t)CAAR(lst);
                scm_string_t rhs = (scm_string_t)CDAR(lst);
                bsize += strlen(lhs->name) + strlen(rhs->name) + 1 + 1;
            } else {
                return NULL;
            }
            lst = CDR(lst);
        }
        if (lst != scm_nil) return NULL;
        scm_bvector_t bv = make_bvector(vm->m_heap, bsize);
        char** index = (char**)bv->elts;
        char* datum = (char*)(index + count + 1);
        lst = env;
        int i = 0;
        while (PAIRP(lst)) {
            scm_string_t lhs = (scm_string_t)CAAR(lst);
            scm_string_t rhs = (scm_string_t)CDAR(lst);
            index[i++] = datum;
            strcpy(datum, lhs->name);
            datum += strlen(lhs->name);
            *datum++ = '=';
            strcpy(datum, rhs->name);
            datum += strlen(rhs->name);
            *datum++ = '\0';
            lst = CDR(lst);
        }
        index[i] = NULL;
        assert((uintptr_t)datum - (uintptr_t)index == bsize);
        return bv;
    }
    return NULL;
}

// process-spawn
#if NO_POSIX_SPAWN
    scm_obj_t
    subr_process_spawn(VM* vm, int argc, scm_obj_t argv[])
    {
        int pipe0[2] = { -1, -1 };
        int pipe1[2] = { -1, -1 };
        int pipe2[2] = { -1, -1 };
        const char* sysfunc = NULL;
        if (argc >= 6) {
            if (argv[0] != scm_true) {  // search
                invalid_argument_violation(vm, "process-spawn", "should be #t on this platform, but got", argv[0], 0, argc, argv);
                return scm_undef;
            }
            if (argv[1] != scm_false) { // environment
                invalid_argument_violation(vm, "process-spawn", "should be #f on this platform, but got", argv[1], 1, argc, argv);
                return scm_undef;
            }
            if (argv[2] != scm_false) { // stdin
                invalid_argument_violation(vm, "process-spawn", "should be #f on this platform, but got", argv[2], 2, argc, argv);
                return scm_undef;
            }
            if (argv[3] != scm_false) { // stdout
                invalid_argument_violation(vm, "process-spawn", "should be #f on this platform, but got", argv[3], 3, argc, argv);
                return scm_undef;
            }
            if (argv[4] != scm_false) { // stderr
                invalid_argument_violation(vm, "process-spawn", "should be #f on this platform, but got", argv[4], 4, argc, argv);
                return scm_undef;
            }
            for (int i = 5; i < argc; i++) {
                if (!STRINGP(argv[i])) {
                    wrong_type_argument_violation(vm, "process-spawn", i, "string", argv[i], argc, argv);
                    return scm_undef;
                }
            }
            sysfunc = "sysconf";
            int open_max;
            if ((open_max = sysconf(_SC_OPEN_MAX)) < 0) goto sysconf_fail;
            sysfunc = "pipe";
            if (pipe(pipe0)) goto pipe_fail;
            if (pipe(pipe1)) goto pipe_fail;
            if (pipe(pipe2)) goto pipe_fail;
            sysfunc = "fork";
            pid_t cpid = fork();
            if (cpid == -1) goto fork_fail;
            if (cpid == 0) {
                if (close(pipe0[1])) goto close_fail;
                if (close(pipe1[0])) goto close_fail;
                if (close(pipe2[0])) goto close_fail;
                if (close(0)) goto close_fail;
                if (dup(pipe0[0]) == -1) goto dup_fail;
                if (close(1)) goto close_fail;
                if (dup(pipe1[1]) == -1) goto dup_fail;
                if (close(2)) goto close_fail;
                if (dup(pipe2[1]) == -1) goto dup_fail;
                for (int i = 3; i < open_max; i++) close(i);
                const char* command_name = ((scm_string_t)argv[5])->name;
                char** command_argv = (char**)alloca(sizeof(char*) * (argc - 5 + 1));
                for (int i = 5; i < argc; i++) command_argv[i - 5] = ((scm_string_t)argv[i])->name;
                command_argv[argc - 5] = (char*)NULL;
                execvp(command_name, command_argv);
                goto exec_fail;
            } else {
                close(pipe0[0]);
                close(pipe1[1]);
                close(pipe2[1]);
                assert(sizeof(pid_t) == sizeof(int));
                return make_list(vm->m_heap,
                                 4,
                                 int_to_integer(vm->m_heap, cpid),
                                 make_std_port(vm->m_heap,
                                               pipe0[1],
                                               make_string_literal(vm->m_heap, "process-stdin"),
                                               SCM_PORT_DIRECTION_OUT,
                                               SCM_PORT_FILE_OPTION_NONE,
                                               SCM_PORT_BUFFER_MODE_BLOCK,
                                               scm_false),
                                 make_std_port(vm->m_heap,
                                               pipe1[0],
                                               make_string_literal(vm->m_heap, "process-stdout"),
                                               SCM_PORT_DIRECTION_IN,
                                               SCM_PORT_FILE_OPTION_NONE,
                                               SCM_PORT_BUFFER_MODE_BLOCK,
                                               scm_false),
                                 make_std_port(vm->m_heap,
                                               pipe2[0],
                                               make_string_literal(vm->m_heap, "process-stderr"),
                                               SCM_PORT_DIRECTION_IN,
                                               SCM_PORT_FILE_OPTION_NONE,
                                               SCM_PORT_BUFFER_MODE_BLOCK,
                                               scm_false));
            }
        }
        wrong_number_of_arguments_violation(vm, "process-spawn", 1, -1, argc, argv);
        return scm_undef;

    sysconf_fail:
    pipe_fail:
    fork_fail:
        {
            int err = errno;
            char message[256];
            snprintf(message, sizeof(message), "%s() failed. %s", sysfunc, strerror(err));
            if (pipe0[0] != -1) close(pipe0[0]);
            if (pipe0[1] != -1) close(pipe0[1]);
            if (pipe1[0] != -1) close(pipe1[0]);
            if (pipe1[1] != -1) close(pipe1[1]);
            if (pipe2[0] != -1) close(pipe2[0]);
            if (pipe2[1] != -1) close(pipe2[1]);
            raise_error(vm, "process-spawn", message, err, argc, argv);
            return scm_undef;
        }

    close_fail:
    dup_fail:
    exec_fail:
        exit(127);
    }
#else
    scm_obj_t
    subr_process_spawn(VM* vm, int argc, scm_obj_t argv[])
    {
        fd_t fd0 = INVALID_FD;
        fd_t fd1 = INVALID_FD;
        fd_t fd2 = INVALID_FD;
        int pipe0[2] = { INVALID_FD, INVALID_FD };
        int pipe1[2] = { INVALID_FD, INVALID_FD };
        int pipe2[2] = { INVALID_FD, INVALID_FD };
        const char* sysfunc = NULL;
        int res = 0;
        if (argc >= 6) {
            if (!BOOLP(argv[0])) {      // search
                wrong_type_argument_violation(vm, "process-spawn", 0, "#t or #f", argv[0], argc, argv);
                return scm_undef;
            }
            bool search = (argv[0] == scm_true);
            scm_bvector_t env = NULL;
            if (argv[1] != scm_false) { // environment
                env = make_posix_env(vm, argv[1]);
                if (env == NULL) {
                    wrong_type_argument_violation(vm, "process-spawn", 1, "#f or environment variable alist", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            if (argv[2] != scm_false) { // stdin
                if (PORTP(argv[2])) {
                    scm_port_t port = (scm_port_t)argv[2];
                    switch (port->type) {
                        case SCM_PORT_TYPE_NAMED_FILE: fd0 = port->fd; break;
                        case SCM_PORT_TYPE_SOCKET: fd0 = ((scm_socket_t)port_socket(port))->fd; break;
                    }
                } else if (SOCKETP(argv[2])) {
                    fd0 = ((scm_socket_t)argv[2])->fd;
                }
                if (fd0 == INVALID_FD) {
                    wrong_type_argument_violation(vm, "process-spawn", 2, "#f, opened file port, socket port, or socket", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (argv[3] != scm_false) { // stdout
                if (PORTP(argv[3])) {
                    scm_port_t port = (scm_port_t)argv[3];
                    switch (port->type) {
                        case SCM_PORT_TYPE_NAMED_FILE: fd1 = port->fd; break;
                        case SCM_PORT_TYPE_SOCKET: fd1 = ((scm_socket_t)port_socket(port))->fd; break;
                    }
                } else if (SOCKETP(argv[3])) {
                    fd1 = ((scm_socket_t)argv[3])->fd;
                }
                if (fd1 == INVALID_FD) {
                    wrong_type_argument_violation(vm, "process-spawn", 3, "#f, opened file port, socket port, or socket", argv[3], argc, argv);
                    return scm_undef;
                }
            }
            if (argv[4] != scm_false) { // stderr
                if (PORTP(argv[4])) {
                    scm_port_t port = (scm_port_t)argv[4];
                    switch (port->type) {
                        case SCM_PORT_TYPE_NAMED_FILE: fd2 = port->fd; break;
                        case SCM_PORT_TYPE_SOCKET: fd2 = ((scm_socket_t)port_socket(port))->fd; break;
                    }
                } else if (SOCKETP(argv[4])) {
                    fd2 = ((scm_socket_t)argv[4])->fd;
                }
                if (fd2 == INVALID_FD) {
                    wrong_type_argument_violation(vm, "process-spawn", 4, "#f, opened file port, socket port, or socket", argv[4], argc, argv);
                    return scm_undef;
                }
            }
            for (int i = 5; i < argc; i++) {
                if (!STRINGP(argv[i])) {
                    wrong_type_argument_violation(vm, "process-spawn", i, "string", argv[i], argc, argv);
                    return scm_undef;
                }
            }
            int b = (fd0 != INVALID_FD && fd0 != 0) + ((fd1 != INVALID_FD && fd1 != 1) << 1);
            bool extra_fd = (fd1 == 0 && (b & 0x01)) || (fd2 == 0 && (b & 0x01)) || (fd2 == 1 && (b & 0x02));
            if (extra_fd) {
                sysfunc = "dup";
                if (fd0 != INVALID_FD) while (fd0 < 3) if ((fd0 = dup(fd0)) == -1) goto dup_fail;
                if (fd1 != INVALID_FD) while (fd1 < 3) if ((fd1 = dup(fd1)) == -1) goto dup_fail;
                if (fd2 != INVALID_FD) while (fd2 < 3) if ((fd2 = dup(fd2)) == -1) goto dup_fail;
            }
            sysfunc = "pipe";
            if (fd0 == INVALID_FD && pipe(pipe0)) goto pipe_fail;
            if (fd1 == INVALID_FD && pipe(pipe1)) goto pipe_fail;
            if (fd2 == INVALID_FD && pipe(pipe2)) goto pipe_fail;
            const char* command_name = ((scm_string_t)argv[5])->name;
            char** command_argv = (char**)alloca(sizeof(char*) * (argc - 5 + 1));
            for (int i = 5; i < argc; i++) command_argv[i - 5] = ((scm_string_t)argv[i])->name;
            command_argv[argc - 5] = (char*)NULL;
            posix_spawn_file_actions_t file_actions;
            posix_spawn_file_actions_init(&file_actions);
            sysfunc = "posix_spawn_file_actions_addclose";
            if (pipe0[1] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe0[1])) != 0) goto addclose_fail;
            if (pipe1[0] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe1[0])) != 0) goto addclose_fail;
            if (pipe2[0] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe2[0])) != 0) goto addclose_fail;
            sysfunc = "posix_spawn_file_actions_adddup2";
            if ((res = posix_spawn_file_actions_adddup2(&file_actions, (pipe0[0] != INVALID_FD ? pipe0[0] : fd0), 0)) != 0) goto adddup2_fail;
            if ((res = posix_spawn_file_actions_adddup2(&file_actions, (pipe1[1] != INVALID_FD ? pipe1[1] : fd1), 1)) != 0) goto adddup2_fail;
            if ((res = posix_spawn_file_actions_adddup2(&file_actions, (pipe2[1] != INVALID_FD ? pipe2[1] : fd2), 2)) != 0) goto adddup2_fail;
            sysfunc = "posix_spawn_file_actions_addclose";
            if (pipe0[0] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe0[0])) != 0) goto addclose_fail;
            if (pipe1[1] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe1[1])) != 0) goto addclose_fail;
            if (pipe2[1] != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, pipe2[1])) != 0) goto addclose_fail;
            if (extra_fd) {
                if (fd0 != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, fd0)) != 0) goto addclose_fail;
                if (fd1 != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, fd1)) != 0) goto addclose_fail;
                if (fd2 != INVALID_FD && (res = posix_spawn_file_actions_addclose(&file_actions, fd2)) != 0) goto addclose_fail;
            }
            pid_t cpid;
            if (search) {
                sysfunc = "posix_spawnp";
                res = posix_spawnp(&cpid, command_name, &file_actions, NULL, command_argv, env ? (char**)env->elts : environ);
            } else {
                sysfunc = "posix_spawn";
                res = posix_spawn(&cpid, command_name, &file_actions, NULL, command_argv, env ? (char**)env->elts : environ);
            }
            posix_spawn_file_actions_destroy(&file_actions);
            if (res) goto spawn_fail;
            sysfunc = "close";
            if (pipe0[0] != INVALID_FD && close(pipe0[0])) goto close_fail;
            if (pipe1[1] != INVALID_FD && close(pipe1[1])) goto close_fail;
            if (pipe2[1] != INVALID_FD && close(pipe2[1])) goto close_fail;
            if (extra_fd) {
                if (fd0 != INVALID_FD && close(fd0)) goto close_fail;
                if (fd1 != INVALID_FD && close(fd1)) goto close_fail;
                if (fd2 != INVALID_FD && close(fd2)) goto close_fail;
            }
            assert(sizeof(pid_t) == sizeof(int));
            return make_list(vm->m_heap,
                             4,
                             int_to_integer(vm->m_heap, cpid),
                             (pipe0[1] != INVALID_FD)
                                ? make_std_port(vm->m_heap,
                                    pipe0[1],
                                    make_string_literal(vm->m_heap, "process-stdin"),
                                    SCM_PORT_DIRECTION_OUT,
                                    SCM_PORT_FILE_OPTION_NONE,
                                    SCM_PORT_BUFFER_MODE_BLOCK,
                                    scm_false)
                                : scm_false,
                             (pipe1[0] != INVALID_FD)
                                ? make_std_port(vm->m_heap,
                                    pipe1[0],
                                    make_string_literal(vm->m_heap, "process-stdout"),
                                    SCM_PORT_DIRECTION_IN,
                                    SCM_PORT_FILE_OPTION_NONE,
                                    SCM_PORT_BUFFER_MODE_BLOCK,
                                    scm_false)
                                : scm_false,
                             (pipe2[0] != INVALID_FD)
                                ? make_std_port(vm->m_heap,
                                    pipe2[0],
                                    make_string_literal(vm->m_heap, "process-stderr"),
                                    SCM_PORT_DIRECTION_IN,
                                    SCM_PORT_FILE_OPTION_NONE,
                                    SCM_PORT_BUFFER_MODE_BLOCK,
                                    scm_false)
                                : scm_false);
        }
        wrong_number_of_arguments_violation(vm, "process-spawn", 6, -1, argc, argv);
        return scm_undef;

        pipe_fail: close_fail: dup_fail:
            res = errno;
        spawn_fail: addclose_fail: adddup2_fail: {
            char message[256];
            snprintf(message, sizeof(message), "%s() failed. %s", sysfunc, strerror(res));
            if (pipe0[0] != INVALID_FD) close(pipe0[0]);
            if (pipe0[1] != INVALID_FD) close(pipe0[1]);
            if (pipe1[0] != INVALID_FD) close(pipe1[0]);
            if (pipe1[1] != INVALID_FD) close(pipe1[1]);
            if (pipe2[0] != INVALID_FD) close(pipe2[0]);
            if (pipe2[1] != INVALID_FD) close(pipe2[1]);
            raise_error(vm, "process-spawn", message, res, argc, argv);
            return scm_undef;
        }
    }
#endif

// process-wait
scm_obj_t
subr_process_wait(VM* vm, int argc, scm_obj_t argv[])
{
    int option = 0;
    if (argc == 2) {
        if (exact_integer_pred(argv[0])) {
            if (BOOLP(argv[1])) {
                if (argv[1] == scm_true) option = WNOHANG;
            } else {
                wrong_type_argument_violation(vm, "process-wait", 1, "#t or #f", argv[1], argc, argv);
                return scm_undef;
            }
            int status;
            int pid;
            if (exact_integer_to_int(argv[0], &pid)) {
                while (true) {
                    int pid2 = waitpid(pid, &status, option);
                    if (pid2 == -1) {
                        if (errno == EINTR) continue;
                        goto waitpid_fail;
                    }
                    if (pid2 != pid) {
                        if (option == WNOHANG) return scm_false;
                        continue;
                    }
                    break;
                }
                if (WIFEXITED(status)) return int_to_integer(vm->m_heap, WEXITSTATUS(status));
                if (WIFSIGNALED(status)) return int_to_integer(vm->m_heap, -WTERMSIG(status));
                return scm_false;
            }
            invalid_argument_violation(vm, "process-wait", "value out of bounds,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "process-wait", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "process-wait", 2, 2, argc, argv);
    return scm_undef;

waitpid_fail:
    int err = errno;
    char message[256];
    snprintf(message, sizeof(message), "waitpid() failed. %s", strerror(err));
    raise_error(vm, "process-wait", message, err);
    return scm_undef;
}


// lookup-process-environment
scm_obj_t
subr_lookup_process_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            const char* value = getenv(string->name);
            if (value) return make_string_literal(vm->m_heap, value);
            return scm_false;
        }
        wrong_type_argument_violation(vm, "lookup-process-environment", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookup-process-environment", 1, 1, argc, argv);
    return scm_undef;
}

// process-environment->alist
scm_obj_t
subr_process_environment_alist(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        extern char **environ;
        char** p = environ;
        scm_obj_t obj = scm_nil;
        while (*p) {
            const char* s = *p;
            const char* e = strchr(s, '=');
            if (e) {
                obj = make_pair(vm->m_heap,
                                make_pair(vm->m_heap,
                                          make_string_literal(vm->m_heap, s, e - s),
                                          make_string_literal(vm->m_heap, e + 1)),
                                obj);
            }
            p++;
        }
        return obj;
    }
    wrong_number_of_arguments_violation(vm, "process-environment->alist", 0, 0, argc, argv);
    return scm_undef;
}

// getenv
scm_obj_t
subr_getenv(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) return subr_lookup_process_environment(vm, argc, argv);
        wrong_type_argument_violation(vm, "getenv", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "getenv", 1, 1, argc, argv);
    return scm_undef;
}

// system
scm_obj_t
subr_system(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            int retval = system(string->name);
            if (retval != -1) {
                if (WIFEXITED(retval)) return int_to_integer(vm->m_heap, WEXITSTATUS(retval));
                if (WIFSIGNALED(retval)) return int_to_integer(vm->m_heap, -WTERMSIG(retval));
            }
            int err = errno;
            char message[256];
            snprintf(message, sizeof(message), "system() failed. %s", strerror(err));
            raise_error(vm, "system", message, err, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "system", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "system", 1, 1, argc, argv);
    return scm_undef;
}

// errno/string
scm_obj_t
subr_errno_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        scm_values_t values = make_values(vm->m_heap, 2);
        values->elts[0] = MAKEFIXNUM(errno);
        values->elts[1] = make_string(vm->m_heap, std::strerror(errno));
        return values;
    }
    wrong_number_of_arguments_violation(vm, "strerror", 0, 0, argc, argv);
    return scm_undef;
}

void
init_subr_process(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("system",subr_system);
    DEFSUBR("process-spawn",subr_process_spawn);
    DEFSUBR("process-wait", subr_process_wait);
    DEFSUBR("getenv",subr_getenv);
    DEFSUBR("errno/string", subr_errno_string);
    DEFSUBR("lookup-process-environment", subr_lookup_process_environment);
    DEFSUBR("process-environment->alist", subr_process_environment_alist);
}
