#include "../scheme.h"
#include <map>
#include <list>
#include <vector>
const ucs4char* UC(const char *str)
{
	typedef std::map<const char *, ucs4char*> Hash;
	typedef std::list<std::vector<ucs4char> > Data;
	static Hash hash;
	static Data data;
	Hash::iterator i = hash.find(str);
	if (i == hash.end()) {
		size_t len = strlen(str);
		data.push_back(std::vector<ucs4char>());
		std::vector<ucs4char>& d = data.back();
		d.resize(len + 1);
		for (size_t i = 0; i < len; i++) {
			d[i] = str[i];
		}
		d[len] = 0;
		hash[str] = &d[0];
		return &d[0];
	} else {
		return i->second;
	}
}

#include "include/gettimeofday.h"
#include <sys/timeb.h>

int gettimeofday(struct timeval *tv, struct timezone *)
{
    struct _timeb timeb;
    _ftime_s(&timeb);
    tv->tv_sec = timeb.time;
    tv->tv_usec = timeb.millitm * 1000;
#pragma message("tz is not supported in gettimeofday")
	return 0;
}
