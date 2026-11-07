#!chezscheme

(library (chezscheme-ffi v1 ffi)
  (export char*
          string->ftype-char*/null
          ftype-null
          ftype-alloc
          ftype-calloc
          ftype-free
          ftype-offset
          calloc
          memcpy
          define-ftype-enum
          define-foreign-procedure)

  (import (chezscheme))

  (define-ftype char* (* char))

  (define (string->ftype-char*/null s)
    (meta-cond
      [(= (optimize-level) 0)
        (unless (string? s)
          (assertion-violationf 'string->ftype-char*/null
            "expected (string? s) but got ~s" s))])
    (let ([fptr (ftype-alloc (add1 (string-length s)) char)])
      (ftype-set! char () fptr (string-length s) #\nul)
      (do ([i (sub1 (string-length s)) (sub1 i)])
          ((negative? i) fptr)
        (ftype-set! char () fptr i (string-ref s i)))))

  (define-syntax ftype-null
    (syntax-rules ()
      [(_ ftype) (make-ftype-pointer ftype 0)]))

  (define-syntax ftype-alloc
    (syntax-rules ()
      [(_ ftype) (ftype-alloc 1 ftype)]
      [(_ length ftype) (make-ftype-pointer ftype (foreign-alloc (* length (ftype-sizeof ftype))))]))

  (define (ftype-free ftype)
    (meta-cond
      [(= (optimize-level) 0)
        (unless (ftype-pointer? ftype)
          (assertion-violationf 'ftype-free
            "expected (ftype-pointer? ftype) but got ~s" ftype))])
    (foreign-free (ftype-pointer-address ftype)))

  (define get-calloc (delay (foreign-procedure "calloc" (size_t size_t) uptr)))

  (define (calloc length size)
    ((get-calloc) length size))

  (define get-memcpy (delay (foreign-procedure "memcpy" (uptr uptr size_t) void)))

  (define (memcpy dst src count)
    ((get-memcpy) dst src count))

  (define-syntax ftype-calloc
    (syntax-rules ()
      [(_ ftype) (ftype-calloc 1 ftype)]
      [(_ length ftype) (make-ftype-pointer ftype (calloc length (ftype-sizeof ftype)))]))

  (define-syntax ftype-offset
    (syntax-rules ()
      [(_ ftype path) (ftype-pointer-address (ftype-&ref ftype path (ftype-null ftype)))]))

  (define-syntax (define-ftype-enum code)
    (syntax-case code ()
      [(_ name repr clause clause* ...)
        #`(begin
            (define-ftype name repr)
            #,(let loop ([i 0] [clause+ #'(clause clause* ...)])
                (syntax-case clause+ ()
                  [() #'(begin)]
                  [([field-name] k/v* ...)
                    #`(begin
                        (define-syntax field-name (identifier-syntax #,i))
                        #,(loop (add1 i) #'(k/v* ...)))]
                  [([field-name val-expr] k/v* ...)
                    #`(begin
                        (define-syntax field-name (identifier-syntax val-expr))
                        #,(loop (add1 (datum val-expr)) #'(k/v* ...)))])))]))

  (define-syntax (define-foreign-procedure code)
    (syntax-case code ()
      [(m name-sym (param-type ...) res-type)
        #`(m name-sym #,(symbol->string (datum name-sym)) (param-type ...) res-type)]
      [(_ name-sym name-str (param-type ...) res-type)
        (begin
          (unless (symbol? (datum name-sym)) (syntax-error #'name-sym "\x1B;[31mexpected foreign name literal to be symbol\x1B;[0m but got"))
          (let ([arg-syms (map
                            (lambda (i) (datum->syntax #'name-sym (gensym (format "arg-~a" i))))
                            (iota (length #'(param-type ...))))])
            #`(begin
                (define get-foreign-procedure (delay (foreign-procedure name-str (param-type ...) res-type)))
                (define (name-sym #,@arg-syms) ((get-foreign-procedure) #,@arg-syms)))))]))

  (load-shared-object #f)

  (meta-cond
    [(= (optimize-level) 0)
      (assert (foreign-entry? "calloc"))
      (assert (foreign-entry? "memcpy"))]))
