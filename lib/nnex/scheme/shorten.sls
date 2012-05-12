(library (nnex scheme shorten)
         (export ^)
         (import (scheme base))
         (begin
;;

(define-syntax ^
  (syntax-rules ()
    ((_ rest ...)
     (lambda rest ...))))

))
