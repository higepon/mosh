#include <windows.h>
#include <stdio.h>
#include <string>
#include <map>

typedef std::map<std::string, std::string> Env;


void getEnv(Env& out)
{
	const char *env = GetEnvironmentStrings();
	for (;;) {
		const char *p = strchr(env + (*env == '=' ? 1 : 0), '=');
		std::string key, val;
		if (p) {
			key.assign(env, p);
			size_t len = strlen(p + 1);
			val.assign(p + 1, len);
			env = p + 1 + len + 1;
			out[key] = val;
		} else {
			return;
		}
		if (*env == 0) break;
	}
}

void put(const Env& env)
{
	for (Env::const_iterator i = env.begin(), iend = env.end(); i != iend; ++i) {
		printf("%s=%s\n", i->first.c_str(), i->second.c_str());
	}
}

int main()
{
	Env env;
	getEnv(env);
	put(env);
}