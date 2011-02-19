#include <stdio.h>
#include <windows.h>

bool isUTF16Console(HANDLE hdl)
{
    return GetFileType(hdl) == FILE_TYPE_CHAR;
}

void put(const char *msg, HANDLE hdl)
{
    fprintf(stderr, "%11s %08x %d\n", msg, hdl, isUTF16Console(hdl));
}

int main()
{
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE realIn = CreateFileA("CONIN$", GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    HANDLE realOut = CreateFileA("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    put("stdin", in);
    put("stdout", out);
    put("real stdin", realIn);
    put("real stdout", realOut);
}
