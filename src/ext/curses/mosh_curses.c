#include "config.h"
#include <curses.h>

#if 0
MOSHEXPORT
void*
mcur_initscr(void){
#ifdef XCURSES /* for PDCurses ...*/
    return Xinitscr();
#else
    return initscr();
#endif

}
#endif /* don't use */

#if 0
MOSHEXPORT
void
mcur_endwin(void){
    endwin();
}
#endif /* don't use */

MOSHEXPORT
int
mcur_getch(void){
    return getch();
}

MOSHEXPORT
void 
mcur_acquire(void){
#ifdef XCURSES /* for PDCurses ...*/
    return Xinitscr();
#else
    return initscr();
#endif
    raw();
    noecho();
    cbreak();
    nonl();
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);
}

MOSHEXPORT
void
mcur_release(void){
    endwin();
}

MOSHEXPORT
void
mcur_refresh(void){
    refresh();
}


