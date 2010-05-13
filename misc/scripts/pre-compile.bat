setlocal
set MOSH_GENSYM_PREFIX=L
del *.sls.mosh-fasl /s
del *.ss.mosh-fasl /s
%1 misc\scripts\pre-compile-r6rs.scm misc\scripts\pre-compile-target-win.scm
