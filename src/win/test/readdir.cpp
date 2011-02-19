#include <stdio.h>
#include <string>
#include <windows.h>

int main(int argc, char *argv[])
{
	if (argc == 1) return 1;

	WIN32_FIND_DATA data;
	HANDLE hdl;

	std::string path(argv[1]);
	printf("path=%s\n", path.c_str());

//	path += "\\*";
	hdl = FindFirstFile(path.c_str(), &data);
	if (hdl == INVALID_HANDLE_VALUE) {
		printf("err=%d\n", GetLastError());
		return 1;
	}

	do {
		printf("%s\n", data.cFileName);
	} while (FindNextFile(hdl, &data));
	FindClose(hdl);
}
