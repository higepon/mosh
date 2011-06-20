(library (yuni text config)
         (export file->config
                 config->file
                 template-replace)
         (import (rnrs)
                 (yuni util files)
                 (yuni text config template)
                 (yuni text config reader)
                 (yuni text config writer))

(define (file->config pth)
  (string-list->config (file->string-list pth)))

)
