(win32-misc
  c-function-table
  *internal*
  (libname: win32-misc)
  (header: "aio_win32.h")
  #(ret name args)
  (int win32_get_processor_count)
  (int win32_get_ansi_codepage)
  (int win32_multibyte_to_widechar (int void* int void* int void*))
  (int win32_measure_multibyte_to_widechar (int void* int))
  (int win32_mypath (void* int)))
