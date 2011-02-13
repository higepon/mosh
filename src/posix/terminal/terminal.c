#ifndef WIN32

#include <unistd.h>

#include <stdlib.h>
#include <stdio.h>

#include <termios.h>
#include <string.h>

#include <sys/ioctl.h> /* non posix */

/* from: http://www.adl.nii.ac.jp/~moro/unix-programmer/faq-j_4.html#SEC48 */

static struct termios stored_settings;

void 
terminal_acquire(void){
    struct termios new_settings;
    tcgetattr(0,&stored_settings);
    new_settings = stored_settings;
    new_settings.c_lflag &= (~ECHO); /* no echo */
    new_settings.c_lflag &= (~ICANON); /* no lined input */
    new_settings.c_lflag &= (~ISIG); /* no signal generation */
    new_settings.c_cc[VTIME] = 0;
    new_settings.c_cc[VMIN] = 1;
    tcsetattr(0,TCSANOW,&new_settings);
    return;
}

void 
terminal_release(void){
    tcsetattr(0,TCSANOW,&stored_settings);
    return;
}

int
terminal_getsize(void){
    struct winsize winsz;
    ioctl(1, TIOCGWINSZ, &winsz);
    return winsz.ws_col;
}

int
terminal_isatty(int fd){
    return isatty(fd);
}
#endif /* ifndef WIN32 */
