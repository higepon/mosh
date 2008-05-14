#include <iostream>
#include <fstream>
#include <cstdlib>
#include <map>
#include <ext/hash_map>
#include <vector>
#include <algorithm>
#include <sys/mman.h>
#include <stdio.h>
#include <utmp.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <tr1/unordered_map>
#include <errno.h>


using namespace std;

struct ltstr
{
    bool operator()(const char* s1, const char* s2) const
  {
      return strcmp(s1, s2);
  }
};


//typedef __gnu_cxx::hash_map<string, int> smap;
typedef __gnu_cxx::hash_map<string, int> smap;
//typedef tr1::unordered_map<string, int> smap;
//typedef __gnu_cxx::hash_map<int, int> smap;
//typedef map<string, int> smap;
//typedef map<int, int> smap;
namespace __gnu_cxx
{
        template<> struct hash< std::string >
        {
                size_t operator()( const std::string& x ) const
                {
                        return hash< const char* >()( x.c_str() );
                }
        };
}


bool compare(const pair<int, int> a, const pair<int, int> b) {
    return a.second > b.second;
}

int hash(const char* str)
{
    int length = strlen(str);
    int ret = 0;
    while (length-- > 0)
    {
        ret = (ret << 5) - ret + ((unsigned char)*str++);
    }
    return ret;
}

char* readline(int& pos, int size, char* p) {
    int org = pos;
    for (int i = 0; i + pos < size; i++) {
        if (p[pos + i] == '\n') {
            p[pos + i] = '\0';
            pos += i + 1;
            return &p[org];
        }
    }
    return NULL;
}

inline void inc(smap& m, const string& key) __attribute__((always_inline));

inline void inc(smap& m, const string& key)
{
    smap::iterator it = m.find(key);
    if (it == m.end()) {
        m.insert(pair<string, int>(key, 1));
    } else {
        m.insert(pair<string, int>(key, ++((*it).second)));
    }
}

#include "../DebugInstruction.h"

const int A = DebugInstruction::COUNT;
const int B = (DebugInstruction::OPERAND_MAX + 2);

inline int index(int insn1, int op1, int insn2, int op2, int insn3, int op3)
{
    return insn1 + (op1 * A) + (insn2 * A * B) + (op2 * A * B * A) + (insn3 * A * B * A * B) + (op3 * A * B * A * B * A);
//    return op3 + insn3 * A + op2 * A * B + insn2 * A * B * A + op1 * A * B * A * B+ insn1 * A * B * A * B * A;
//     return insn1
//         + op1   * DebugInstruction::COUNT
//         + insn2 * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1)
//         + op2   * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1) * DebugInstruction::COUNT
//         + insn3 * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1) * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1)
//         + op3   * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1) * DebugInstruction::COUNT * (DebugInstruction::OPERAND_MAX + 1) * DebugInstruction::COUNT;
}

inline int get(int* table, int insn1, int op1, int insn2, int op2, int insn3, int op3)
{
    return table[index(insn1, op1, insn2, op2, insn3, op3)];
}

inline void inc(int* table, int insn1, int op1, int insn2, int op2, int insn3, int op3)
{
    const int i = index(insn1, op1, insn2, op2, insn3, op3);
    table[i] = table[i] + 1;
}

#define NONE (DebugInstruction::OPERAND_MAX + 1)

inline void incAll(int* table, int insn1, int op1, int insn2, int op2, int insn3, int op3) __attribute__((always_inline));

inline void incAll(int* table, int insn1, int op1, int insn2, int op2, int insn3, int op3)
{
    if (op1 == NONE && op2 == NONE && op2 == NONE) {
        inc(table, insn1, op1, insn2, op2, insn3, op3);


    } else if (op1 != NONE && op2 == NONE && op3 == NONE) {

        inc(table, insn1, op1, insn2, op2, insn3, op3);

    } else if (op1 != NONE && op2 != NONE && op3 == NONE) {
        inc(table, insn1, op1, insn2, op2, insn3, op3);
        inc(table, insn1, NONE, insn2, op2, insn3, op3);
        inc(table, insn1, op1, insn2, NONE, insn3, op3);
        inc(table, insn1, NONE, insn2, NONE, insn3, op3);

    } else if (op1 != NONE && op2 != NONE && op3 != NONE) {
        inc(table, insn1, op1, insn2, op2, insn3, op3);
        inc(table, insn1, NONE, insn2, op2, insn3, op3);
        inc(table, insn1, op1, insn2, NONE, insn3, op3);
        inc(table, insn1, NONE, insn2, NONE, insn3, op3);
        inc(table, insn1, op1, insn2, op2, insn3, NONE);
        inc(table, insn1, NONE, insn2, op2, insn3, NONE);
        inc(table, insn1, op1, insn2, NONE, insn3, NONE);
        inc(table, insn1, NONE, insn2, NONE, insn3, NONE);

    } else if (op1 != NONE && op2 == NONE && op3 != NONE) {
        inc(table, insn1, op1, insn2, op2, insn3, op3);
        inc(table, insn1, NONE, insn2, op2, insn3, op3);
        inc(table, insn1, NONE, insn2, op2, insn3, NONE);
        inc(table, insn1, op1, insn2, op2, insn3, NONE);


    } else if (op1 == NONE && op2 != NONE && op3 != NONE) {
        inc(table, insn1, op1, insn2, op2, insn3, op3);
        inc(table, insn1, op1, insn2, NONE, insn3, op3);
        inc(table, insn1, op1, insn2, NONE, insn3, NONE);
        inc(table, insn1, op1, insn2, op2, insn3, NONE);
    }

}

inline void decode(int index, int& insn1, int& op1, int& insn2, int& op2, int& insn3, int& op3)
{
    op3 = index / (A * B * A * B * A);
    index = (index % (A * B * A * B * A));

    insn3 = index / (A * B * A * B);
    index = index % (A * B * A * B);

    op2 = index / (A * B * A);
    index = index % (A * B * A);

    insn2 = index / (A * B);
    index = index % (A * B);

    op1 = index / (A);
    index = index % (A);

    insn1 = index;
}

int main() {
#if 0
    for (int a = 0; a < A; a++) {
    for (int b = 0; b < B; b++) {
    for (int c = 0; c < A; c++) {
    for (int d = 0; d < B; d++) {
    for (int e = 0; e < A; e++) {
    for (int f = 0; f < B; f++) {
        int aa, bb, cc, dd, ee, ff;
        int v = index(a, b, c, d, e, f);
        decode(v, aa, bb, cc, dd, ee, ff);
        if (a != aa ||
            b != bb ||
            c != cc ||
            d != dd ||
            e != ee ||
            f != ff) {
            printf("!!!! (a=(%d, %d), b=(%d, %d), c=(%d, %d) d=(%d, %d), e=(%d, %d), f=(%d, %d))\n", a, aa, b, bb, c, cc, d, dd, e, ee, f, ff);
        }

    }}}}}}
    exit(-1);
#endif
//    {
//     int insn1 = 0;
//     int insn2 = 0;
//     int insn3 = 0;
//     int arg1  = 0;
//     int arg2  = 0;
//     int arg3  = 0;

//     int q = index(DebugInstruction::REFER_GLOBAL, 0
// ,
//                   DebugInstruction::REFER_GLOBAL, 1,
//                   DebugInstruction::HALT, 3);
//     decode(q, insn1, arg1, insn2, arg2, insn3, arg3);
//         printf("(%s[%d], %s[%d], %s[%d])\n"
//                , DebugInstruction::toString(insn1)
//                , arg1
//                , DebugInstruction::toString(insn2)
//                , arg2
//                , DebugInstruction::toString(insn3)
//                , arg3);
        
//         return 0;
//            }

#if 1
    const int tableSize = DebugInstruction::COUNT *
        DebugInstruction::COUNT *
        DebugInstruction::COUNT *
        (DebugInstruction::OPERAND_MAX + 2) *
        (DebugInstruction::OPERAND_MAX + 2) *
        (DebugInstruction::OPERAND_MAX + 2);
    int* table = new int[tableSize];
    memset(table, 0, DebugInstruction::COUNT *
                         DebugInstruction::COUNT *
                         DebugInstruction::COUNT *
                         (DebugInstruction::OPERAND_MAX + 1) *
                         (DebugInstruction::OPERAND_MAX + 1) *
           (DebugInstruction::OPERAND_MAX + 1));

    smap m;
    fstream file;
    file.open("./instruction.log", ios::binary | ios::in);
    if (!file.is_open()) {
        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
        return -1;
    }

    int insn1 = 0;
    int insn2 = 0;
    int insn3 = 0;
    int arg1  = 0;
    int arg2  = 0;
    int arg3  = 0;

    string str;
    int total = 0;
    while (!file.eof()) {
        total++;
//        printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//         if (str.empty()) continue;
//         string::size_type delimiter = str.find('_');
//         if (delimiter == string::npos) {
//             insn3 = strtol(str.c_str(), (char **) NULL, 10);
//             if (errno == ERANGE) {
//                 printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//             }
//             arg3  = NONE;
//         } else {
//             insn3 = strtol(str.substr(0, delimiter).c_str(), (char **) NULL, 10);
//             if (errno == ERANGE) {
//                 printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//             }

//             arg3 = strtol(str.substr(delimiter + 1).c_str(), (char **) NULL, 10);
//             if (errno == ERANGE) {
//                 printf("%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
//             }

//         }
        uint8_t buf[2];
        file.read((char*)buf, 2);
        insn3 = buf[0];
        arg3 = buf[1];

//         printf("(%s, %s, %s)\n"
//                , DebugInstruction::toString(insn1)
//                , DebugInstruction::toString(insn2)
//                , DebugInstruction::toString(insn3));

        incAll(table, insn1, arg1, insn2, arg2, insn3, arg3);
        incAll(table, insn2, arg2, insn3, arg3, DebugInstruction::NO_INSN, NONE);
        if (arg3 != NONE) incAll(table, insn3, arg3,  DebugInstruction::NO_INSN, NONE, DebugInstruction::NO_INSN, NONE);
        insn1 = insn2;
        insn2 = insn3;
        arg1 = arg2;
        arg2 = arg3;
    }

    vector< pair <int, int> > v;
    for (int i = 0; i < tableSize; i++) {
        const int val = table[i];
        if (val > 1000000) {
            v.push_back(pair<int, int>(i, val));
        }
    }
    sort(v.begin(), v.end(), compare);
    int i = 0;
    printf("(total %d)\n", total);
    for (i = 0; i < 20 && i < v.size(); i++) {
        decode(v[i].first, insn1, arg1, insn2, arg2, insn3, arg3);
        char buf[32];
        string i1(DebugInstruction::toString(insn1));
        string i2(DebugInstruction::toString(insn2));
        string i3(DebugInstruction::toString(insn3));
        printf("((%s", i1.c_str());
        if (arg1 != NONE) {
            printf(" %c", arg1 + '0');
        }
        if (i2 != "") {
            printf(" => %s", i2.c_str());
            if (arg2 != NONE) {
                printf(" %c", arg2 + '0');
            }
        }
        if (i3 != "") {
            printf(" => %s", i3.c_str());
            if (arg3 != NONE) {
                printf(" %c", arg3 + '0');
            }
        }
        printf(") %d %2.2f)\n", v[i].second, 100.0 * v[i].second / total);

//         printf("(%s %c %s %c %s %c %d)\n"
//                , DebugInstruction::toString(insn1), arg1 == NONE ? ' ' : arg1 + '0'
//                , DebugInstruction::toString(insn2), arg2 == NONE ? ' ' : arg2 + '0'
//                , DebugInstruction::toString(insn3), arg3 == NONE ? ' ' : arg3 + '0', v[i].second);
    }
    printf("\n\n");
    file.close();
    exit(0);
#else
    smap m;
    fstream file;
    file.open("./instruction.log");
    if (!file.is_open()) {
        return -1;
    }

    string prev, prevOp;
    string prev2, prev2Op;
    int prevArg;
    int prev2Arg;
    int total = 0;
    string str;
    while (getline(file, str)) {
        string op;
        int arg;
        const char b = *(str.end() - 2);
        const char c = *(str.end() - 1);
        if ('0' <= c && c <= '9' && b == '_') {
            arg = c - '0';
            op = str.substr(0, str.size() - 1);
        } else {
            arg = -1;
        }

        if (-1 == arg) {
            if (-1 == prevArg) {
                const string a = prev + str;
                const string b = prev2 + a;
                inc(m, a);
                if (-1 == prev2Arg) {
                    inc(m, b);
                } else {
                    inc(m, b);
                    inc(m, prev2Op + a);
                }
            } else {
                const string a =  prevOp + str;
                const string b = prev + str;
                inc(m, a);
                inc(m, b);
                if (-1 == prev2Arg) {
                    inc(m, prev2 + a);
                    inc(m, prev2 + b);
                } else {
                    inc(m, prev2 + a);
                    inc(m, prev2 + b);
                    inc(m, prev2Op + a);
                    inc(m, prev2Op + b);
                }
            }
        } else {
            inc(m, str);
            if (-1 == prevArg) {
                const string a = prev + op;
                const string b = prev + str;
                inc(m, a);
                inc(m, b);
                if (-1 == prev2Arg) {
                    inc(m, prev2 + a);
                    inc(m, prev2 +b);
                } else {
                    inc(m, prev2 + a);
                    inc(m, prev2 +b);
                    inc(m, prev2Op + a);
                    inc(m, prev2Op +b);
                }
            } else {
                const string a = prev + op;
                const string b = prev + str;
                const string c = prevOp + op;
                const string d = prevOp + str;
                inc(m, a);
                inc(m, b);
                inc(m, c);
                inc(m, d);
                if (-1 == prev2Arg) {
                    inc(m, prev2 + a);
                    inc(m, prev2 + b);
                    inc(m, prev2 + c);
                    inc(m, prev2 + d);
                } else {
                    inc(m, prev2 + a);
                    inc(m, prev2 + b);
                    inc(m, prev2 + c);
                    inc(m, prev2 + d);
                    inc(m, prev2Op + a);
                    inc(m, prev2Op + b);
                    inc(m, prev2Op + c);
                    inc(m, prev2Op + d);

                }
            }
        }
        prev2 = prev;
        prev2Op = prevOp;
        prev2Arg = prevArg;
        prevArg = arg;
        prev = str;
        prevOp = op;
        total++;
    }

    vector< pair<string, int> > v(10000);
    for (smap::const_iterator it = m.begin(); it != m.end(); ++it) {
        v.push_back(pair<string, int>((*it).first, (*it).second));
    }

    sort(v.begin(), v.end(), compare);
    printf("(total %d)\n", total);
    int i = 0;
    for (i = 0; i < 5 && i < v.size(); i++) {
        printf("((%s) %d %2.2f)\n", v[i].first.c_str(), v[i].second, 100.0 * v[i].second / total);
//        cout << v[i].first << " " << v[i].second << endl;
    }

//     string str;
//     string prev;
//     string prev2;

//    int total  = 0;
//    while (getline(file, str)) {
//         const string key = prev + "-" + str;
//         smap::iterator it = m.find(key);
//         if (it == m.end()) {
//             m.insert(pair<string, int>(key, 1));

//         } else {
//             m.insert(pair<string, int>(key, ++((*it).second)));
//         }
//         const string key2 = prev2 + "-" + prev + "-" + str;
//         it = m.find(key2);
//         if (it == m.end()) {
//             m.insert(pair<string, int>(key2, 1));

//         } else {
//             m.insert(pair<string, int>(key2, ++((*it).second)));
//         }

//        prev2 = prev;
//        prev = str;
//        total++;
//    }

// //   vector< pair<string, int> > v;
//     vector< pair<string, int> > v;
//     for (smap::const_iterator it = m.begin(); it != m.end(); ++it) {
//         v.push_back(pair<string, int>((*it).first, (*it).second));
//     }

//     sort(v.begin(), v.end(), compare);
//     file.close();

//     printf("(total %d)\n", total);
//     int i = 0;
//     for (i = 0; i < 5 && i < v.size(); i++) {
//         printf("((%s) %d %2.2f)\n", v[i].first.c_str(), v[i].second, 100.0 * v[i].second / total);
// //        cout << v[i].first << " " << v[i].second << endl;
//     }
#endif
}
