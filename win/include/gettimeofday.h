#ifndef WIN_INCLUDE_GETTIMEOFDAY_H_
#define WIN_INCLUDE_GETTIMEOFDAY_H_

// for win32
#include <time.h>

struct timeval {
	time_t tv_sec;
	int tv_usec;
};

struct timezone {
	int tz_minuteswest;
	int tz_dsttime;
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

#endif
