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

void* return_pointer_string()
{
  return "hello";
}

void* return_array_of_pointer_string()
{
  static char* p[2];
  p[0] = "hello";
  p[1] = "world";
  return (void*)p;
}
