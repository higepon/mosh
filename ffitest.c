#include <ctype.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <stdio.h>

int return3()
{
  return 3;
}

typedef struct Struct1 {
  int a;
  int b;
  int c;
} Struct1;

void fillStruct1(Struct1* s)
{
    s->a = 1234;
    s->b = -2;
    s->c = 30;
}

int sizeOfStruct1() {
  return sizeof(Struct1);
}

int aOfStruct1(Struct1* s) {
  return s->a;
}

int bOfStruct1(Struct1* s) {
  return s->b;
}

int cOfStruct1(Struct1* s) {
  return s->c;
}


// for x86-64
// 6 register arguments + 2 stack arguments
int add8(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8)
{
  return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8;
}

int subf(double a, double b)
{
  return a - b;
}

int double10(double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10)
{
  return x1 - x2 + x3 - x4 + x5 - x6 + x7 - x8 + x9 - x10;
}


double double10_2(double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10)
{
  return x1 - x2 + x3 - x4 + x5 - x6 + x7 - x8 + x9 - x10;
}


double subf2(double a, double b)
{
  return a - b;
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

char* return_pointer_null()
{
  return NULL;
}

void* return_array_of_pointer_string()
{
  static char* p[2];
  p[0] = "hello";
  p[1] = "world";
  return (void*)p;
}

typedef struct FFITestStruct {
  char charVal;
  unsigned char ucharVal;

} FFITestStruct;

void* return_struct()
{
  static FFITestStruct st;
  st.charVal = -1;
  st.ucharVal = 255 ;
  return &st;
}
char struct_ref(FFITestStruct* st)
{
  return st->charVal;
}

typedef struct FFITestDoubleStruct {
  double val;
} FFITestDoubleStruct;

void* return_double_struct()
{
  static FFITestDoubleStruct st;
  st.val = 3.14;
  return &st;
}

typedef struct FFITestLongLongStruct {
  long long val;
} FFITestLongLongStruct;

void* return_longlong_struct()
{
  static FFITestLongLongStruct st;
  st.val = 123456789123456789LL;
  return &st;
}

typedef struct FFITestUint8tStruct {
  uint8_t val;
} FFITestUint8tStruct;

void* return_uint8_t_struct()
{
  static FFITestUint8tStruct st;
  st.val = 130;
  return &st;
}

typedef struct FFITestUint64tStruct {
  uint64_t val;
} FFITestUint64tStruct;

void* return_uint64_t_struct()
{
  static FFITestUint64tStruct st;
  st.val = 130;
  return &st;
}


char* append_hello(const char* text)
{
  static char buf[256];
  snprintf(buf, 256, "%shello", text);
  return buf;
}

void change_errno()
{
  errno = 3;
}

void abc(void* p)
{
  strcpy(p, "abc");
}

int callCallback0(int (*func) (void))
{
  return func();
}

int callCallback1(int (*func) (int))
{
  return func(0x1234);
}

int callCallback2(int (*func) (int, int))
{
  return func(0x1235, 0x1234);
}

int callCallback3(int (*func) (double, int))
{
  return func(123.4, 100);
}

double callCallback4(double (*func) (double, double))
{
  return func(123.5, 10.0);
}

int64_t callCallback5(int64_t (*func) (int64_t))
{
  const int64_t v = 0x1234567887654321;
  return func(v);
}
