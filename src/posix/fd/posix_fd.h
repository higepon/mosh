#ifdef __cplusplus
extern "C" {
#endif

int fd_read(int fd,void* buf,int len);
int fd_write(int fd,void* buf,int len);
int fd_close(int fd);
void fd_setnonblock(int fd);
int fd_pipe(int* in,int* out);

#ifdef __cplusplus
}
#endif
