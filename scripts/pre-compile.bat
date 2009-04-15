setlocal
set MOSH_GENSYM_PREFIX=L
del *.sls.fasl /s
del *.ss.fasl /s
%1 scripts\pre-compile-r6rs.scm scripts\pre-compile-target-win.scm
