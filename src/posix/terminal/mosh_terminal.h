#ifndef _MOSH_TERMINAL_H
extern "C"{
void terminal_acquire(void);
void terminal_release(void);
int terminal_getsize(void);
int terminal_isatty(int);
};
#endif
