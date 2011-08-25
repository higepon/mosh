#ifdef __cplusplus
extern "C" {
#endif

int kq_create(void);
void* kevent_alloc(int n);
void* kevent_offset(void* ke,int n);
void kevent_dispose(void* p);
void kevent_set_readevent(void* ke,int fd);
void kevent_set_writeevent(void* ke,int fd);
void kevent_set_enableuserevent(void* ke,int id);
void kevent_set_triggeruserevent(void* ke,int id);
int kevent_ident(void* ke);
int kevent_type(void* ke);
void kevent_decode_fd(void* ke,int* type,int* eofp,int* data);
int kevent_exec(int q,int changecount, void* ke_changes,int count, void *ke_out,int timeout_ms);
int socket_getaddrinfo(char* name,char* servicename, void* ret_addrinfo, int mode, int proto);
int socket_sizeof_sockaddr_storage(void);
int socket_create(int mode,int proto);
void socket_freeaddrinfo(void* ai);
int socket_bind(int fd,void* name,int len);
int socket_listen(int fd,int l);
int socket_connect(int fd,void* name,int len);
int socket_accept(int fd,void* name,unsigned int* len);
void socket_addrinfo_read(void* aip,int *ret_family,void** ret_addr,int* ret_len,void** ret_next);
void socket_setnodelay(int fd);

#ifdef __cplusplus
}
#endif
