#ifndef WIN_INCLUDE_GETTIMEOFDAY_H_
#define WIN_INCLUDE_GETTIMEOFDAY_H_

// for win32
#include <time.h>
#include <winsock.h> // for timeval

#if 0
struct timeval {
	time_t tv_sec;
	int tv_usec;
};
#endif

struct timezone {
	int tz_minuteswest;
	int tz_dsttime;
};

int gettimeofday(struct timeval *tv, struct timezone *tz);

#endif
