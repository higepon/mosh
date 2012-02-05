#include "config.h"

#include <windows.h>
#include <mmsystem.h>

MOSHEXPORT
int /* count */
mmm_joy_count(void){
    return joyGetNumDevs();
}

MOSHEXPORT
int /* bool */
mmm_joy_read(int id,void* p){
    int res;
    JOYINFOEX* joy = (JOYINFOEX *)p;
    joy->dwSize = sizeof(JOYINFOEX);
    joy->dwFlags = JOY_RETURNALL;

    res = joyGetPosEx(id,joy);
    if(res != JOYERR_NOERROR){
        return 0;
    }else{
        return 1;
    }
}
