#!/usr/local/bin/mosh
(import (rnrs)
        (mosh cgi)
        (srfi :39)
        (lambda wiki))

(parameterize ([wiki-data-direcoty "."]
               [wiki-top-url "http://localhost:8001"])
  (wiki-main))
