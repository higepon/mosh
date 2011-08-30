#ifdef __cplusplus
extern "C"{
#endif
int debugee_spawn(int,void*,void*,void*,void*);
int debugee_fileactionssize(void);
void debugee_fileactions_init(void*);
void debugee_fileactions_destroy(void*);
void debugee_fileactions_adddup2(void*,int,int);
void debugee_fileactions_addclose(void*,int);
#ifdef __cplusplus
};
#endif

