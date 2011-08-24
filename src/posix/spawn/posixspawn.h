#ifdef __cplusplus
extern "C"{
#endif
int posixspawn_spawn(void*,void*,void*,void*,void*);
int posixspawn_fileactionssize(void);
void posixspawn_fileactions_init(void*);
void posixspawn_fileactions_destroy(void*);
void posixspawn_fileactions_adddup2(void*,int,int);
void posixspawn_fileactions_addclose(void*,int);
#ifdef __cplusplus
};
#endif

