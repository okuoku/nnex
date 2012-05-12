;; From nmosh
(library (nnex library file)
         (export library-name->filename)
         (import (nnex scheme)
                 (nnex os)
                 (nnex library config))

;;
(define ERRPORT current-error-port)
(define DEBUGGING #f)
(define (PCK . obj)
  (if %verbose
    (begin 
      (if (not DEBUGGING)
        (begin 
          (display "-> " (ERRPORT))
          (for-each (lambda (e)
                      (display e (ERRPORT))
                      (display " " (ERRPORT)))
                    obj)
          (newline (ERRPORT)))))))

(define (DEBUGMODE-ON)
  (set! DEBUGGING #t))

;;------------------------------------------------
;; library aliasing
;;------------------------------------------------
(define library-rename-table '())
(define (set-library-rename-table! spec)
  (set! library-rename-table spec))

(define (rename-library spec)
  (let ((x (assoc spec library-rename-table)))
    (cond
      (x 
        (let ((newname (cdr x)))
          (PCK "alias: " spec "=>" newname)
          newname))
      (else 
        ;(PCK "not renamed: " spec)
        spec))))

;;------------------------------------------------
;; definitions
;;------------------------------------------------

(define (run-win32-np?) (string=? "win32" (host-os)))
(define CHR-ENVPATHSEP (if (run-win32-np?) #\; #\:))

(define pathfilter 
  (if (run-win32-np?) 
    (lambda (str) 
      (and (string? str) 
           (list->string (map (lambda (e) (if (char=? e #\\) #\/ e)) 
                              (string->list str)))))
    (lambda (str) str)))

(define pathfinish 
  (if (run-win32-np?)
    (lambda (str) (and (string? str) (list->string (cdr (string->list str)))))
    (lambda (str) str)))

;; Win32 path hack.

(define (make-extended-path str)
  ;; Win32 extened path won't allow slash as path-sep.
  ;; So we map them to back-slashes
  (string-append "\\\\?\\"
                 (list->string
                   (map (lambda (c) (if (char=? c #\/) #\\ c))
                        (string->list str)))))

(define path-absolute
  (if (run-win32-np?)
    (lambda (str)
      (let ((head (and (< 1 (string-length str))
                       (substring str 0 2))))
        (if (and head (string=? "\\\\" head))
          str ;; return the path as-is if the path wasn't a absolute local path.
          (make-extended-path str))))
    (lambda (str) str)))


(define absolute-path? 
  (if (run-win32-np?)
    (lambda (pl)
      (let ((a (car pl)))
        (or
          (and ; is a drive letter?
            (= (string-length a) 2)
            (char=? (cadr (string->list a)) #\:))
          ;; ... or UNC path ?
          (= (string-length a) 0))))
    (lambda (pl) (= 0 (string-length (car pl))) )))

;;------------------------------------------------
;; utils
;;------------------------------------------------
(define (strsep str chr)
  (define (gather l) ;
    (define (itr cur rest0 rest1)
      (cond
        ((not (pair? rest1)) (reverse cur))
        (else
          (itr (cons (substring str
                                (+ 1 (car rest0)) 
                                (car rest1)) cur) 
               (cdr rest0) 
               (cdr rest1)))))
    (itr '() l (cdr l)))
  (define (spl l s)
    (define (itr idx cur rest)
      (cond
        ((not (pair? rest)) (reverse (cons idx cur)))
        ((char=? s (car rest))
         (itr (+ idx 1) (cons idx cur) (cdr rest)))
        (else
          (itr (+ idx 1) cur (cdr rest)))))
    (itr 0 (list -1) l))
  (if (string? str)
    (let* ((l (string->list str))
           (m (spl l chr))
           (r (gather m)))
      r )
    '()
    ))

;;------------------------------------------------
;; path handling
;;------------------------------------------------
(define RUNPATH (pathfilter (current-directory)))

(define (compose-path l)
  (define (fold-dotdot l)
    (define (itr cur rest)
      (if (pair? rest)
        (let ((a (car rest)))
          (if (string=? ".." a)
            (itr (cdr cur) (cdr rest)) ; drop top
            (itr (cons a cur) (cdr rest))))
        (reverse cur)))
    (itr '() l))
  (define (omit-dot l)
    (define (itr cur rest)
      (if (pair? rest)
        (let ((a (car rest)))
          (if (string=? "." a)
            (itr cur (cdr rest)) ; drop "."
            (itr (cons a cur) (cdr rest))))
        (reverse cur)))
    (itr '() l))
  (define (omit-zerolen l)
    (define (itr cur rest)
      (if (pair? rest)
        (let ((a (car rest)))
          (if (= 0 (string-length a))
            (itr cur (cdr rest))
            (itr (cons a cur) (cdr rest))))
        (reverse cur)))
    (itr '() l))
  (define (insert-slash l)
    (define (itr cur rest)
      (if (pair? rest)
        (itr (cons "/" (cons (car rest) cur)) (cdr rest))
        (reverse (cdr cur)))) ;drop last "/"
    (itr (list "/") l))

  ;(PCK 'COMPOSING: l)
  (apply string-append 
         (insert-slash (fold-dotdot (omit-dot (omit-zerolen l))))))

(define make-absolute-path
  (if (run-win32-np?)
    (lambda (pth)
      (define pl (strsep (pathfilter pth) #\/))
      (cond
        ((and
           pth
           (< 4 (string-length pth))
           (string=? "\\\\?\\" (substring pth 0 4)))
         ;; If it was extended path, return it as-is
         pth)
        (else
          (if (pair? pl) 
            (path-absolute
              (pathfinish
                (compose-path
                  ;; Standard absolute path (c:\hoge\fuga)
                  (if (and (< 2 (string-length pth))
                           (char=? #\: (string-ref pth 1)))
                    pl
                    (append (strsep RUNPATH #\/) pl)))))
            ""))))
    (lambda (pth)
      ;; FIXME: To avoid psyntax-mosh bug
      (define pl (strsep (pathfilter pth) #\/))
      (if (pair? pl)
        (pathfinish
          (compose-path
            (if (absolute-path? pl)
              (cdr pl)
              (append (strsep RUNPATH #\/) pl))))
        ""))))

(define (pathsep str)
  (strsep str CHR-ENVPATHSEP))

(define (for-each1-tail proc lst)
  (cond
    ((and (pair? lst) (pair? (cdr lst)))
     (proc (car lst))
     (for-each1-tail proc (cdr lst)))
    ((and (pair? lst) (null? (cdr lst)))
     (proc (car lst)))))
    

(define (make-prefix-list)
  (define (append-prefix-x l str)
    (cond
      ((and str (file-exists? str))
       (map (lambda (e) (string-append (pathfilter str) "/" e)) l))
      (else '())))
  (define (append-prefix-l l lstr)
    (define (itr cur rest)
      (cond
        ((not (pair? rest)) cur)
        (else
          ;(PCK 'append-prefix-itr (car rest))
          (itr (append cur (append-prefix-x l (car rest))) (cdr rest)))))
    (itr '() lstr))

  (define (append-prefix-curpath l)
    (append-prefix-x l (current-directory)))
  (define (append-prefix-execpath l)
    (append-prefix-x l (executable-path)))
  (define (append-prefix-execpath-rel l)
    (let ((pth (executable-path)))
      (if pth
        (append-prefix-x l (string-append pth (standard-library-path)))
        '())))
  (define (append-prefix-stdlibpath l)
    (append-prefix-x l (standard-library-path)))
  (define (append-prefix-loadpath l)
    (let ((var (get-environment-variable "MOSH_LOADPATH")))
      ;(PCK 'var var)
      (append-prefix-l l (pathsep var))))
  (define (append-prefix l)
    (append
      ;(append-prefix-execpath l)
      (if %loadpath (append-prefix-l l (pathsep %loadpath)) '())
      (if (get-environment-variable "MOSH_LOADPATH")
        (append-prefix-loadpath l)
        '())
      (append-prefix-curpath l)
      (if %prefixless-mode '() (append-prefix-stdlibpath l))
      l ;fallback
      (append-prefix-execpath-rel l) ; fallback
      ))
  (append-prefix (list "" "lib/")))

(define prefix-list (make-prefix-list))

; TODO: support multiple libraries
(define (library-name->filename name) ; => PATH or #f
  (define (expand-prefix str l)
    (map (lambda (e) (string-append e str)) l))
  (define (expand-suffix str l)
    (map (lambda (e) (string-append str e)) l))
  (define (nibblechar n)
    (cond
      ((<= 0 n 9) (integer->char (+ n #x30)))
      (else (integer->char (+ (- 10 n) #x61)))))
  (define (between? x y z) ; Gauche does not support 3 args char<=?
    ; ... But this is no longer true for Gauche 0.9.2
    (and (char<=? x y)
         (char<=? y z)))
  (define (namesymbol s)
    (define (convc c)
      (cond ;;from psyntax
        ((or (between? #\a c #\z)
             (between? #\A c #\Z)
             (between? #\0 c #\9)
             ;(append-prefix-curpath l)
             (memv c '(#\- #\. #\_ #\~))) (list c))
        (else (let ((i (char->integer c)))
                (list
                  #\%
                  (nibblechar (quotient i 16))
                  (nibblechar (remainder i 16)))))))
    (list->string (apply append (map convc (string->list (symbol->string s))))))

  (define (check-files l)
    (if (null? l)
      #f
      (begin
        ;(PCK 'check-file (make-absolute-path (car l)))
        (if (file-exists? (make-absolute-path (car l)))
          (car l)
          (check-files (cdr l))))))
  ; e.g. for (example test) :
  ; 1. build base (example test) => "example/test"
  (define (basename name)
    (define (itr cur l)
      (if (null? l)
        cur
        (itr (string-append cur "/" (namesymbol (car l))) (cdr l))))
    (itr (namesymbol (car name)) (cdr name)))
  ; 2. expand pre/sufx
  (define (expand-name name)
    (apply append
           (map (lambda (s) 
                  (expand-suffix s 
                                 '(".nmosh.sls" ".nmosh.ss" ".nmosh.scm"
                                   ".mosh.sls" ".mosh.ss" ".mosh.scm"
                                   ".sls" ".ss" ".scm")))
                (expand-prefix name prefix-list))))

  (let* ((fn (check-files (expand-name (basename name))))
         (cfn (make-absolute-path fn)))
    (if fn
      (begin
        ;(PCK 'PATH fn '=> cfn)
        cfn)
      #f))
  )

)
