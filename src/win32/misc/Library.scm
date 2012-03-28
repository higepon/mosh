(win32-misc
  c-function-table
  *internal*
  (libname: win32-misc)
  (header: "aio_win32.h")
  #(ret name args)
  (void win32_invoke_ffithread (void* void* void* void* void*))
  (int win32_get_processor_count)
  (int win32_get_ansi_codepage)
  (int win32_multibyte_to_widechar (int void* int void* int void*))
  (int win32_measure_multibyte_to_widechar (int void* int))
  (int win32_mypath (void* int))
  (int win32_setenv (void* void*))
  (int win32_querydosdevice (void* void* int))
  (int win32_extent_size (int))
  (int win32_extent_get (void* void* int void*))
  (int win32_extent_disknumber (void* int))
  (void win32_extent_offset (void* int void* void*))
  (void win32_extent_length (void* int void* void*)))
