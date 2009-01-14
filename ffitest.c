#include <ctype.h>
#include <string.h>
int return3()
{
  return 3;
}

int sub(int a, int b)
{
  return a - b;
}

int sub3(int a, int b, int c)
{
  return a - b - c;
}

int string_length(const char* text)
{
    return strlen(text);
}

void my_upper(char* text, int length)
{
    int i;
    for (i = 0; i < length; i++) {
        text[i] = toupper(text[i]);
    }
}

void* pointer()
{
  return (void*)0x12345678;
}
