(library (yuni scheme refimpl arithmetic r6rs flonums)
         (export    
           flonum?
           ;real->flonum
           fl=? fl<? fl>? fl<=? fl>=?
           flinteger? flzero? flpositive? flnegative? flodd? fleven?
           flfinite? flinfinite? flnan?
           flmax flmin
           fl+ fl* fl- fl/
           flabs
           fldiv-and-mod fldiv flmod
           ;fldiv0-and-mod0 fldiv0 flmod0
           ;flnumerator fldenominator
           flfloor flceiling fltruncate flround
           flexp fllog flsin flcos fltan flasin flacos flatan
           flsqrt flexpt
           )
         (import (yuni scheme refimpl arithmetic impl flonum)))

