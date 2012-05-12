(library (nnex library config)
         (export 
           ;; Flags
           %verbose
           %prefixless-mode
           %portable-mode
           ;; Constants
           %loadpath

           ;; Parameters
           standard-library-path
           executable-path)
         (import 
           (nnex scheme)
           (primitives 
             %verbose
             %loadpath
             %nmosh-prefixless-mode
             %nmosh-portable-mode
             standard-library-path
             executable-path))

(define %prefixless-mode %nmosh-prefixless-mode)
(define %portable-mode %nmosh-portable-mode)

)
