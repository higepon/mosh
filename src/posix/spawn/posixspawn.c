#include "config.h"
#ifdef HAVE_POSIX_SPAWN

#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include "posixspawn.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>

int
posixspawn_spawn(void* out_pid,void* in_path,void* in_fileactions,void* in_argv,void* in_envp){
    short flags = 0;
    sigset_t mask;
    posix_spawnattr_t attr;
    posix_spawnattr_init(&attr);
/* took from Ruby's posix-spawn */
    flags |= POSIX_SPAWN_SETSIGMASK;
    sigemptyset(&mask);
    posix_spawnattr_setsigmask(&attr,&mask);
#if defined(POSIX_SPAWN_USEVFORK) || defined(__linux__)
    flags |= POSIX_SPAWN_USEVFORK;
#endif
    posix_spawnattr_setflags(&attr,flags);

    return posix_spawn(
            (int *)out_pid,
            in_path,
            in_fileactions,
            &attr,
            in_argv,in_envp);
}

int
posixspawn_fileactionssize(void){
    return sizeof(posix_spawn_file_actions_t);
}

void
posixspawn_fileactions_init(void* p){
    posix_spawn_file_actions_t* t = (posix_spawn_file_actions_t *)p;
    posix_spawn_file_actions_init(t);
}

void
posixspawn_fileactions_destroy(void* p){
    posix_spawn_file_actions_t* t = (posix_spawn_file_actions_t *)p;
    posix_spawn_file_actions_destroy(t);
}

void
posixspawn_fileactions_adddup2(void* p,int fd0,int fd1){
    posix_spawn_file_actions_t* t = (posix_spawn_file_actions_t *)p;
    posix_spawn_file_actions_adddup2(t,fd0,fd1);
}
void
posixspawn_fileactions_addclose(void* p,int fd){
    posix_spawn_file_actions_t* t = (posix_spawn_file_actions_t *)p;
    posix_spawn_file_actions_addclose(t,fd);
}

#endif
