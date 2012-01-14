#ifndef _AIO_WIN32_H
#define _AIO_WIN32_H
#ifdef __cplusplus
extern "C" {
#endif
uintptr_t win32_iocp_create(void);
int win32_iocp_assoc(uintptr_t iocp,uintptr_t in,uintptr_t key);
int win32_iocp_pop(uintptr_t iocp,intptr_t timeout,uintptr_t ret_bytestrans,uintptr_t ret_key,uintptr_t ret_overlapped);
void* win32_overlapped_alloc(void);
void win32_overlapped_free(void *p);
void* win32_overlapped_getmydata(void* p);
void win32_overlapped_setmydata(void* p,void* q);
int win32_overlapped_geterror(void* p);
int win32_handle_read_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol);
int win32_handle_write_async(uintptr_t h,uintptr_t offsetL,uintptr_t offsetH,uintptr_t length,uintptr_t buf,uintptr_t ol);
uintptr_t win32_process_redirected_child2(wchar_t *spec,wchar_t* dir,wchar_t* std_in,wchar_t* std_out,wchar_t* std_err,int in_enable,int out_enable,int err_enable);
uintptr_t win32_create_named_pipe_async(wchar_t* name);
int win32_wait_named_pipe_async(uintptr_t h,uintptr_t ovl);
int win32_process_wait_async(uintptr_t h,uintptr_t iocp,uintptr_t key,uintptr_t overlapped);
//int win32_process_get_result(void* p);
//int win32_cancelioex(void* h,void* ovl);
int win32_handle_close(void* h);

//winsock
int win32_sockaddr_storage_size(void);
uintptr_t win32_socket_create(int mode,int proto,uintptr_t ret_connectex,uintptr_t ret_acceptex);
int win32_getaddrinfo(wchar_t* name,wchar_t* servicename,uintptr_t ret_addrinfoex,int mode,int proto);
void win32_addrinfoex_free(uintptr_t aie);
void win32_addrinfoex_read(uintptr_t aie,uintptr_t* ret_family,uintptr_t* ret_sockaddr,uintptr_t* ret_namelen,uintptr_t* ret_next);
int win32_socket_connect(uintptr_t func,uintptr_t s,uintptr_t saddr,int namelen,uintptr_t overlapped);
int win32_socket_accept(uintptr_t func,uintptr_t slisten,uintptr_t saccept,uintptr_t buf,int bufsize,uintptr_t overlapped);
int win32_socket_bind(uintptr_t s,uintptr_t name,int namelen);
int win32_socket_listen(uintptr_t s,int l);
int win32_socket_close(uintptr_t s);
int win32_socket_getsockname(uintptr_t s,uintptr_t buf,int len);

// GC related
void* win32_finalization_handler_get(void);
void* win32_finalization_handler_create(void* iocp,void* key,void* ptr);
void win32_finalization_handler_dispose(void* d);


// GUI related (subset)
int win32_messagebox(wchar_t* caption,wchar_t* msg,int dlgtype,int icontype);
void win32_window_move(void* hWnd,signed int x,signed int y,signed int w,signed int h);
void win32_window_show(void* hWnd,int cmd);
void win32_window_hide(void* hWnd);
void win32_window_settitle(void* hWnd,wchar_t* text);
void win32_window_close(void* hWnd);
void win32_window_destroy(void* hWnd);
void win32_registerwindowclass(void);
void* win32_window_alloc(void);
void win32_window_create(void* iocp,void* overlapped);
void win32_window_fitbuffer(void* wnd);

void win32_window_updaterects(void* w,void* dc, int* rects, int count);
void* win32_window_createbitmap(void *w,int x,int y);
void win32_window_getwindowrect(void*,int*,int*,int*,int*);
void win32_window_getclientrect(void*,int*,int*,int*,int*);
int win32_window_getclientrect_x(void* h);
int win32_window_getclientrect_y(void* h);
void* win32_dc_create(void);
void win32_dc_dispose(void* d);
void win32_dc_selectobject(void* d,void* obj);
void win32_dc_transform(void* d,void* m);
void win32_dc_settransform(void* d,void* m);
void win32_gdi_deleteobject(void* obj);
void* win32_pen_create(int w,int r,int g,int b);
void* win32_brush_create(int r,int g,int b);
void* win32_font_create(int h,int weight,int italicp,wchar_t* face);
void win32_dc_draw(void* dc,void* bmpdc,intptr_t* ops,int len);
int win32_dc_measure_text(void* d,wchar_t* str,int len,int* x,int* y);


void win32_getmonitorinfo(int id,int cmd,signed int *valid,signed int *x0,signed int* y0,signed int *x1,signed int *y1);

// misc
void win32_invoke_ffithread(HANDLE iocp,uintptr_t func,uintptr_t in0,uintptr_t in1,uintptr_t overlapped);
int win32_get_processor_count(void);
int win32_get_ansi_codepage(void);
int win32_multibyte_to_widechar(int cp, void* input, int input_count, void* output, int output_count, int* output_size);
int win32_measure_multibyte_to_widechar(int cp, void* input, int input_count);
int win32_mypath(wchar_t* buf,int len);

#ifdef __cplusplus
};
#endif
#endif
