;;; Copyright (c) 2019 Indigo BioAutomation, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(library (protobuf)
  (export
   define-enum
   define-message
   read-message
   write-message
   )
  (import
   (chezscheme)
   (swish imports))

  (define-syntax define-enum
    (syntax-rules ()
      [(_ type (key value) ...)
       (and (identifier? #'type)
            (andmap identifier? #'(key ...))
            (andmap integer? (datum (value ...))))
       (define-syntax (type x)
         (syntax-case x ()
           [(_ s)
            (cond
             [(assq (datum s) '((key . value) ...)) => cdr]
             [else (syntax-error x "unknown enum member")])]))]))

  (define-syntax (define-message x)
    (define (fresh-id prefix stype)
      (datum->syntax stype
        (gensym (format "~a~a" prefix (syntax->datum stype)))))
    (syntax-case x ()
      [(_ type (fname ftype fnumber) ...)
       (and (identifier? #'type) (andmap identifier? #'(fname ...)))
       (with-syntax ([read-type (fresh-id "read-" #'type)]
                     [merge-type (fresh-id "merge-" #'type)]
                     [size-type (fresh-id "size-" #'type)]
                     [write-type (fresh-id "write-" #'type)])
         #'(begin
             (define-tuple type fname ...)
             (define (merge-type a b)
               (if (not a)
                   b
                   (type make [fname (merge-field ftype
                                       (type no-check fname a)
                                       (type no-check fname b))] ...)))
             (define (read-type lip)
               (let read-type ([lip lip] [fname (default-value ftype)] ...)
                 (if (limit-reached? lip)
                     (type make [fname (maybe-reverse fname ftype)] ...)
                     (let-values ([(field-number wire-type) (read-key lip)])
                       (cond
                        [(eqv? field-number fnumber)
                         (let ([fname (read-field fname ftype wire-type lip)])
                           (read-type lip fname ...))]
                        ...
                        [else
                         (skip! wire-type lip)
                         (read-type lip fname ...)])))))
             (define (size-type x ht)
               (type open x (fname ...))
               (cache-size! x ht (+ (size-field fname ftype fnumber ht) ...)))
             (define (write-type x ht op)
               (type open x (fname ...))
               (write-field fname ftype fnumber ht op)
               ...
               (void))
             (define-property type *merger* #'merge-type)
             (define-property type *reader* #'read-type)
             (define-property type *sizer* #'size-type)
             (define-property type *writer* #'write-type)))]))

  (define-syntax read-message
    (syntax-rules ()
      [(_ type ip) (read-message type ip #f)]
      [(_ type ip limit) ($read-message (message-reader type) ip limit)]))

  (define ($read-message reader ip limit)
    (if (bytevector? ip)
        (reader (make-lip (open-bytevector-input-port ip) (bytevector-length ip)))
        (reader (make-lip ip limit))))

  (define-syntax write-message
    (syntax-rules ()
      [(_ type e)
       ($write-message (message-sizer type) (message-writer type) e)]
      [(_ type e op)
       (let ([x e] [ht (make-eq-hashtable)])
         ((message-sizer type) x ht)
         ((message-writer type) x ht op))]
      [(_ type e op size)
       (let ([x e] [ht (make-eq-hashtable)])
         (size ((message-sizer type) x ht))
         ((message-writer type) x ht op))]))

  (define ($write-message sizer writer x)
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([ht (make-eq-hashtable)])
        (sizer x ht)
        (writer x ht op))
      (get)))

  (define *merger*)
  (define *reader*)
  (define *sizer*)
  (define *writer*)

  (define-syntax (message-merger x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ type)
         (or (lookup #'type #'*merger*)
             (syntax-error #'type "undefined message type"))])))

  (define-syntax (message-reader x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ type)
         (or (lookup #'type #'*reader*)
             (syntax-error #'type "undefined message type"))])))

  (define-syntax (message-sizer x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ type)
         (or (lookup #'type #'*sizer*)
             (syntax-error #'type "undefined message type"))])))

  (define-syntax (message-writer x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ type)
         (or (lookup #'type #'*writer*)
             (syntax-error #'type "undefined message type"))])))

  (define-syntax cache-size!
    (syntax-rules ()
      [(_ x ht size)
       (and (identifier? #'x) (identifier? #'ht))
       (or (eq-hashtable-ref ht x #f)
           (let ([n size])
             (eq-hashtable-set! ht x n)
             n))]))

  (define-syntax cached-size
    (syntax-rules ()
      [(_ x ht) (eq-hashtable-ref ht x 'cache-miss)]))

  (define-syntax keyword?
    (syntax-rules ()
      [(_ x) (eq? (datum x) 'x)]))

  (define-syntax default-value
    (syntax-rules ()
      [(_ (message type)) (keyword? message) #f]
      [(_ (repeated type)) (keyword? repeated) '()]
      [(_ (map key-type value-type)) (keyword? map) '()]
      [(_ bool) (keyword? bool) #f]
      [(_ double) (keyword? double) 0.0]
      [(_ float) (keyword? float) 0.0]
      [(_ bytes) (keyword? bytes) '#vu8()]
      [(_ string) (keyword? string) ""]
      [(_ type) 0]))

  (define-syntax maybe-reverse
    (syntax-rules ()
      [(_ x (repeated type)) (keyword? repeated) (reverse x)]
      [(_ x (map key-type value-type)) (keyword? map) (reverse x)]
      [(_ x type) x]))

  (define-syntax merge-field
    (syntax-rules ()
      [(_ (message type) a b)
       (and (keyword? message) (identifier? #'type))
       ((message-merger type) a b)]
      [(_ (repeated type) a b) (keyword? repeated) (append a b)]
      [(_ (map key-type value-type) a b) (keyword? map) (append a b)]
      [(_ type a b) b]))

  (define-syntax read-field
    (syntax-rules ()
      ;; varint scalars
      [(_ x type wire-type lip)
       (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64 bool))
       (begin
         (check-wire-type wire-type wire-type-varint)
         ((scalar-reader type) lip))]
      ;; enum
      [(_ x (enum type) wire-type lip)
       (keyword? enum)
       (read-field x int32 wire-type lip)]
      ;; 64-bit scalars
      [(_ x type wire-type lip)
       (memq (datum type) '(fixed64 sfixed64 double))
       (begin
         (check-wire-type wire-type wire-type-64-bit)
         ((scalar-reader type) lip))]
      ;; string/bytes
      [(_ x type wire-type lip)
       (memq (datum type) '(string bytes))
       (begin
         (check-wire-type wire-type wire-type-length-delimited)
         ((scalar-reader type) lip))]
      ;; 32-bit scalars
      [(_ x type wire-type lip)
       (memq (datum type) '(fixed32 sfixed32 float))
       (begin
         (check-wire-type wire-type wire-type-32-bit)
         ((scalar-reader type) lip))]
      ;; message
      [(_ x (message type) wire-type lip)
       (and (keyword? message) (identifier? #'type))
       (begin
         (check-wire-type wire-type wire-type-length-delimited)
         (read-message-field x (message-merger type) (message-reader type)
           lip))]
      ;; repeated varint scalars
      [(_ x (repeated type) wire-type lip)
       (and (keyword? repeated)
            (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64 bool)))
       (cond
        [(eqv? wire-type wire-type-length-delimited)
         (read-packed-scalar x (scalar-reader type) lip)]
        [else
         (check-wire-type wire-type wire-type-varint)
         (read-unpacked-field x (scalar-reader type) lip)])]
      ;; repeated enum
      [(_ x (repeated (enum type)) wire-type lip)
       (and (keyword? repeated) (keyword? enum))
       (read-field x (repeated int32) wire-type lip)]
      ;; repeated 64-bit scalars
      [(_ x (repeated type) wire-type lip)
       (and (keyword? repeated)
            (memq (datum type) '(fixed64 sfixed64 double)))
       (cond
        [(eqv? wire-type wire-type-length-delimited)
         (read-packed-scalar x (scalar-reader type) lip)]
        [else
         (check-wire-type wire-type wire-type-64-bit)
         (read-unpacked-field x (scalar-reader type) lip)])]
      ;; repeated string/bytes
      [(_ x (repeated type) wire-type lip)
       (and (keyword? repeated) (memq (datum type) '(string bytes)))
       (begin
         (check-wire-type wire-type wire-type-length-delimited)
         (read-unpacked-field x (scalar-reader type) lip))]
      ;; repeated 32-bit scalars
      [(_ x (repeated type) wire-type lip)
       (and (keyword? repeated) (memq (datum type) '(fixed32 sfixed32 float)))
       (cond
        [(eqv? wire-type wire-type-length-delimited)
         (read-packed-scalar x (scalar-reader type) lip)]
        [else
         (check-wire-type wire-type wire-type-32-bit)
         (read-unpacked-field x (scalar-reader type) lip)])]
      ;; repeated message
      [(_ x (repeated (message type)) wire-type lip)
       (and (keyword? repeated) (keyword? message) (identifier? #'type))
       (begin
         (check-wire-type wire-type wire-type-length-delimited)
         (read-message-field x snoc (message-reader type) lip))]
      ;; map
      [(_ x (map key-type value-type) wire-type lip)
       (keyword? map)
       (letrec ([read-map
                 (case-lambda
                  [(lip) (read-map lip
                           (default-value key-type)
                           (default-value value-type))]
                  [(lip key value)
                   (if (limit-reached? lip)
                       (cons key value)
                       (let-values ([(fnumber wire-type) (read-key lip)])
                         (cond
                          [(eqv? fnumber 1)
                           (read-map lip
                             (read-field key key-type wire-type lip) value)]
                          [(eqv? fnumber 2)
                           (read-map lip key
                             (read-field value value-type wire-type lip))]
                          [else
                           (skip! wire-type lip)
                           (read-map lip key value)])))])])
         (check-wire-type wire-type wire-type-length-delimited)
         (read-message-field x snoc read-map lip))]))

  (define-syntax size-field
    (syntax-rules ()
      ;; varint numeric scalars
      [(_ e type fnumber ht)
       (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64))
       (let ([x e])
         (if (eqv? x 0)
             0
             (+ (size-key fnumber) ((scalar-sizer type) x))))]
      ;; enum
      [(_ e (enum type) fnumber ht)
       (keyword? enum)
       (size-field e int32 fnumber ht)]
      ;; bool
      [(_ e bool fnumber ht)
       (keyword? bool)
       (if e (+ (size-key fnumber) 1) 0)]
      ;; 64-bit scalars
      [(_ e type fnumber ht)
       (memq (datum type) '(fixed64 sfixed64 double))
       (if (zero? e) 0 (+ (size-key fnumber) 8))]
      ;; string
      [(_ e string fnumber ht)
       (keyword? string)
       (let ([size (size-string e)])
         (if (eqv? size 0)
             0
             (size-length-delimited fnumber size)))]
      ;; bytes
      [(_ e bytes fnumber ht)
       (keyword? bytes)
       (let ([size (bytevector-length e)])
         (if (eqv? size 0)
             0
             (size-length-delimited fnumber size)))]
      ;; 32-bit scalars
      [(_ e type fnumber ht)
       (memq (datum type) '(fixed32 sfixed32 float))
       (if (zero? e) 0 (+ (size-key fnumber) 4))]
      ;; message
      [(_ e (message type) fnumber ht)
       (and (keyword? message) (identifier? #'type))
       (let ([x e])
         (if x
             (size-length-delimited fnumber ((message-sizer type) x ht))
             0))]
      ;; packed varint numeric scalars
      [(_ e (repeated type) fnumber ht)
       (and (keyword? repeated)
            (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64)))
       (let ([ls e])
         (if (null? ls)
             0
             (size-length-delimited fnumber
               (cache-size! ls ht
                 (fold-left (lambda (size x) (+ size ((scalar-sizer type) x)))
                   0 ls)))))]
      ;; repeated enum
      [(_ e (repeated (enum type)) fnumber ht)
       (and (keyword? repeated) (keyword? enum))
       (size-field e (repeated int32) fnumber ht)]
      ;; packed bool
      [(_ e (repeated bool) fnumber ht)
       (and (keyword? repeated) (keyword? bool))
       (let ([ls e])
         (if (null? ls)
             0
             (size-length-delimited fnumber
               (cache-size! ls ht (length ls)))))]
      ;; packed 64-bit scalars
      [(_ e (repeated type) fnumber ht)
       (and (keyword? repeated) (memq (datum type) '(fixed64 sfixed64 double)))
       (let ([ls e])
         (if (null? ls)
             0
             (size-length-delimited fnumber
               (cache-size! ls ht (* (length ls) 8)))))]
      ;; repeated string/bytes
      [(_ e (repeated type) fnumber ht)
       (and (keyword? repeated) (memq (datum type) '(string bytes)))
       (fold-left
        (lambda (size x)
          (+ size (size-length-delimited fnumber ((scalar-sizer type) x))))
        0 e)]
      ;; packed 32-bit scalars
      [(_ e (repeated type) fnumber ht)
       (and (keyword? repeated) (memq (datum type) '(fixed32 sfixed32 float)))
       (let ([ls e])
         (if (null? ls)
             0
             (size-length-delimited fnumber
               (cache-size! ls ht (* (length ls) 4)))))]
      ;; repeated message
      [(_ e (repeated (message type)) fnumber ht)
       (and (keyword? repeated) (keyword? message) (identifier? #'type))
       (fold-left
        (lambda (size x)
          (+ size (size-length-delimited fnumber ((message-sizer type) x ht))))
        0 e)]
      ;; map
      [(_ e (map key-type value-type) fnumber ht)
       (keyword? map)
       (fold-left
        (lambda (size x)
          (+ size (size-length-delimited fnumber
                    (cache-size! x ht
                      (+ (size-field (car x) key-type 1 ht)
                         (size-field (cdr x) value-type 2 ht))))))
        0 e)]))

  (define-syntax write-field
    (syntax-rules ()
      ;; varint numeric scalars
      [(_ e type fnumber ht op)
       (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64))
       (let ([x e])
         (unless (eqv? x 0)
           (write-key fnumber wire-type-varint op)
           ((scalar-writer type) x op)))]
      ;; enum
      [(_ e (enum type) fnumber ht op)
       (keyword? enum)
       (write-field e int32 fnumber ht op)]
      ;; bool
      [(_ e bool fnumber ht op)
       (keyword? bool)
       (when e
         (write-key fnumber wire-type-varint op)
         (put-u8 op 1))]
      ;; 64-bit scalars
      [(_ e type fnumber ht op)
       (memq (datum type) '(fixed64 sfixed64 double))
       (let ([x e])
         (unless (zero? x)
           (write-key fnumber wire-type-64-bit op)
           ((scalar-writer type) x op)))]
      ;; string
      [(_ e string fnumber ht op)
       (keyword? string)
       (let ([x e])
         (unless (eqv? (string-length x) 0)
           (write-key fnumber wire-type-length-delimited op)
           (write-string x op)))]
      ;; bytes
      [(_ e bytes fnumber ht op)
       (keyword? bytes)
       (let ([x e])
         (unless (eqv? (bytevector-length x) 0)
           (write-key fnumber wire-type-length-delimited op)
           (write-bytes x op)))]
      ;; 32-bit scalars
      [(_ e type fnumber ht op)
       (memq (datum type) '(fixed32 sfixed32 float))
       (let ([x e])
         (unless (zero? x)
           (write-key fnumber wire-type-32-bit op)
           ((scalar-writer type) x op)))]
      ;; message
      [(_ e (message type) fnumber ht op)
       (and (keyword? message) (identifier? #'type))
       (let ([x e])
         (when x
           (write-key fnumber wire-type-length-delimited op)
           (write-varint (cached-size x ht) op)
           ((message-writer type) x ht op)))]
      ;; packed scalars
      [(_ e (repeated type) fnumber ht op)
       (and (keyword? repeated)
            (memq (datum type) '(int32 int64 uint32 uint64 sint32 sint64 bool
                                  fixed64 sfixed64 double
                                  fixed32 sfixed32 float)))
       (let ([ls e])
         (unless (null? ls)
           (write-key fnumber wire-type-length-delimited op)
           (write-varint (cached-size ls ht) op)
           (for-each (lambda (x) ((scalar-writer type) x op)) ls)))]
      ;; repeated enum
      [(_ e (repeated (enum type)) fnumber ht op)
       (and (keyword? repeated) (keyword? enum))
       (write-field e (repeated int32) fnumber ht op)]
      ;; repeated string/bytes
      [(_ e (repeated type) fnumber ht op)
       (and (keyword? repeated) (memq (datum type) '(string bytes)))
       (for-each
        (lambda (x)
          (write-key fnumber wire-type-length-delimited op)
          ((scalar-writer type) x op))
        e)]
      ;; repeated message
      [(_ e (repeated (message type)) fnumber ht op)
       (and (keyword? repeated) (keyword? message)
            (identifier? #'type))
       (for-each
        (lambda (x)
          (write-key fnumber wire-type-length-delimited op)
          (write-varint (cached-size x ht) op)
          ((message-writer type) x ht op))
        e)]
      ;; map
      [(_ e (map key-type value-type) fnumber ht op)
       (keyword? map)
       (for-each
        (lambda (x)
          (write-key fnumber wire-type-length-delimited op)
          (write-varint (cached-size x ht) op)
          (write-field (car x) key-type 1 ht op)
          (write-field (cdr x) value-type 2 ht op))
        e)]))

  (define-syntax (scalar-reader x)
    (syntax-case x ()
      [(k type) (compound-id #'k "read-" #'type)]))

  (define-syntax (scalar-sizer x)
    (syntax-case x ()
      [(k type) (compound-id #'k "size-" #'type)]))

  (define-syntax (scalar-writer x)
    (syntax-case x ()
      [(k type) (compound-id #'k "write-" #'type)]))

  (define-syntax size-length-delimited
    (syntax-rules ()
      [(_ fnumber e)
       (let ([size e]) (+ (size-key fnumber) (size-varint size) size))]))

  ;; lip = limited input port

  (define-syntax make-lip (identifier-syntax cons))
  (define-syntax lip-ip (identifier-syntax #3%car))
  (define-syntax lip-limit (identifier-syntax #3%cdr))
  (define-syntax lip-limit-set! (identifier-syntax #3%set-cdr!))

  (define (read-packed-scalar ls reader lip)
    (define (read-list ls reader lip)
      (if (limit-reached? lip)
          ls
          (read-list (cons (reader lip) ls) reader lip)))
    (let ([size (read-varint lip)])
      (read-list ls reader (new-limit lip size))))

  (define-syntax read-unpacked-field
    (syntax-rules ()
      [(_ ls reader lip) (cons (reader lip) ls)]))

  (define (read-message-field x merger reader lip)
    (let ([size (read-varint lip)])
      (merger x (reader (new-limit lip size)))))

  (define (snoc a b) (cons b a))

  (define (read-key lip)
    (let* ([x (read-varint lip)]
           [key (ash x -3)])
      (when (eqv? key 0)
        (errorf 'read-message "illegal field number 0"))
      (values key (logand x 7))))

  (define-syntax (size-key x)
    (define ($size-varint x)
      (let lp ([x x] [size 1])
        (if (<= x #x7F)
            size
            (lp (ash x -7) (fx+ size 1)))))
    (syntax-case x ()
      [(_ field-number) ($size-varint (ash (datum field-number) 3))]))

  (define-syntax write-key
    (syntax-rules ()
      [(_ field-number wire-type op)
       (write-varint (+ (ash field-number 3) wire-type) op)]))

  (define (read-fixed n lip)
    (if (eqv? n 0)
        n
        (let* ([x (read-u8 lip)]
               [y (read-fixed (- n 1) lip)])
          (+ x (ash y 8)))))

  (define (read-sfixed n lip)
    (let ([x (read-fixed n lip)])
      (if (>= x (ash 1 (- (* n 8) 1)))
          (- x (ash 1 (* n 8)))
          x)))

  (define (write-fixed n x op)
    (when (> n 0)
      (put-u8 op (logand x #xFF))
      (write-fixed (- n 1) (ash x -8) op)))

  (define (check-wire-type wire-type expected)
    (unless (eqv? wire-type expected)
      (errorf 'read-message "unexpected wire-type ~a, expected ~a"
        wire-type expected)))

  (define (limit-reached? lip)
    (let ([limit (lip-limit lip)])
      (or (eqv? limit 0)
          (and (not limit)
               (eof-object? (peek-u8 (lip-ip lip)))))))

  (define (update-limit! lip size)
    (cond
     [(lip-limit lip) =>
      (lambda (limit)
        (if (<= size limit)
            (lip-limit-set! lip (- limit size))
            (errorf 'read-message "size limit exceeded")))]))

  (define (new-limit lip size)
    (update-limit! lip size)
    (make-lip (lip-ip lip) size))

  (define (skip! wire-type lip)
    (cond
     [(eqv? wire-type wire-type-varint) (read-varint lip)]
     [(eqv? wire-type wire-type-64-bit) (skip-u8 8 lip)]
     [(eqv? wire-type wire-type-length-delimited)
      (let ([size (read-varint lip)])
        (skip-u8 size lip))]
     [(eqv? wire-type wire-type-32-bit) (skip-u8 4 lip)]
     [else
      (errorf 'read-message "invalid wire-type ~s" wire-type)]))

  (define (skip-u8 n lip)
    (unless (eqv? n 0)
      (read-u8 lip)
      (skip-u8 (- n 1) lip)))

  (define (peek-u8 ip)
    (let ([x (get-u8 ip)])
      (unless (eof-object? x)
        (unget-u8 ip x))
      x))

  (define (read-u8 lip)
    (let ([limit (lip-limit lip)])
      (when (eqv? limit 0)
        (errorf 'read-message "limit exceeded"))
      (let ([x (get-u8 (lip-ip lip))])
        (when (eof-object? x)
          (errorf 'read-message "unexpected eof"))
        (when limit (lip-limit-set! lip (- limit 1)))
        x)))

  (define (read-varint lip)
    (let ([x (read-u8 lip)])
      (if (<= x #x7F)
          x
          (+ (ash (read-varint lip) 7) (fx- x #x80)))))

  (define (size-varint x)
    (let lp ([x x] [size 1])
      (if (<= x #x7F)
          size
          (lp (ash x -7) (fx+ size 1)))))

  (define (write-varint x op)
    (if (<= x #x7F)
        (put-u8 op x)
        (begin
          (put-u8 op (fx+ (logand x #x7F) #x80))
          (write-varint (ash x -7) op))))

  (define (encode-zigzag x)
    (if (< x 0)
        (- (ash (- x) 1) 1)
        (ash x 1)))

  (define (read-zigzag lip)
    (let ([x (read-varint lip)])
      (if (even? x) (ash x -1) (- (ash (+ x 1) -1)))))

  (define (size-zigzag x)
    (size-varint (encode-zigzag x)))

  (define (write-zigzag x op)
    (write-varint (encode-zigzag x) op))

  (define (int32? x)
    (if (< (fixnum-width) 32)
        (or (fixnum? x)
            (and (bignum? x) (<= #x-100000000 x #x7FFFFFFF)))
        (and (fixnum? x) (fx<= #x-100000000 x #x7FFFFFFF))))

  (define (uint32? x)
    (if (< (fixnum-width) 33)
        (or (and (fixnum? x) (fx<= 0 x))
            (and (bignum? x) (<= 0 x #xFFFFFFFF)))
        (and (fixnum? x) (fx<= 0 x #xFFFFFFFF))))

  (define (int64? x)
    (or (fixnum? x)
        (and (bignum? x) (<= #x-10000000000000000 x #x7FFFFFFFFFFFFFFF))))

  (define (uint64? x)
    (or (and (fixnum? x) (fx<= 0 x))
        (and (bignum? x) (<= 0 x #xFFFFFFFFFFFFFFFF))))

  ;; Varint wire types

  (define-syntax wire-type-varint (identifier-syntax 0))

  (define (read-int32 lip)
    (let ([x (read-uint32 lip)])
      (if (< x (ash 1 31)) x (- x (ash 1 32)))))

  (define (size-int32 x)
    (size-varint (if (< x 0) (logand x #xFFFFFFFF) x)))

  (define (write-int32 x op)
    (unless (int32? x)
      (errorf 'write-message "~s is not an int32" x))
    (write-varint (if (< x 0) (logand x #xFFFFFFFF) x) op))

  (define (read-int64 lip)
    (let ([x (read-uint64 lip)])
      (if (< x (ash 1 63)) x (- x (ash 1 64)))))

  (define (size-int64 x)
    (size-varint (if (< x 0) (logand x #xFFFFFFFFFFFFFFFF) x)))

  (define (write-int64 x op)
    (unless (int64? x)
      (errorf 'write-message "~s is not an int64" x))
    (write-varint (if (< x 0) (logand x #xFFFFFFFFFFFFFFFF) x) op))

  (define (read-uint32 lip) (logand (read-varint lip) #xFFFFFFFF))

  (alias size-uint32 size-varint)

  (define (write-uint32 x op)
    (unless (uint32? x)
      (errorf 'write-message "~s is not a uint32" x))
    (write-varint x op))

  (define (read-uint64 lip) (logand (read-varint lip) #xFFFFFFFFFFFFFFFF))

  (alias size-uint64 size-varint)

  (define (write-uint64 x op)
    (unless (uint64? x)
      (errorf 'write-message "~s is not a uint64" x))
    (write-varint x op))

  (alias read-sint32 read-zigzag)

  (alias size-sint32 size-zigzag)

  (define (write-sint32 x op)
    (unless (int32? x)
      (errorf 'write-message "~s is not a sint32" x))
    (write-zigzag x op))

  (alias read-sint64 read-zigzag)

  (alias size-sint64 size-zigzag)

  (define (write-sint64 x op)
    (unless (int64? x)
      (errorf 'write-message "~s is not a sint64" x))
    (write-zigzag x op))

  (define (read-bool lip) (not (eqv? (read-varint lip) 0)))

  (define-syntax size-bool (identifier-syntax (lambda (x) 1)))

  (define (write-bool x op)
    (put-u8 op (if x 1 0)))

  ;; 64-bit wire types

  (define-syntax wire-type-64-bit (identifier-syntax 1))

  (define (read-double lip)
    (bytevector-ieee-double-ref (read-bv 8 lip) 0 'little))

  (define-syntax size-double (identifier-syntax (lambda (x) 8)))

  (define (write-double x op)
    (let ([bv (make-bytevector 8)])
      (bytevector-ieee-double-set! bv 0 x 'little)
      (put-bytevector op bv)))

  (define (read-fixed64 lip) (read-fixed 8 lip))

  (define-syntax size-fixed64 (identifier-syntax (lambda (x) 8)))

  (define (write-fixed64 x op)
    (unless (uint64? x)
      (errorf 'write-message "~s is not a fixed64" x))
    (write-fixed 8 x op))

  (define (read-sfixed64 lip) (read-sfixed 8 lip))

  (define-syntax size-sfixed64 (identifier-syntax (lambda (x) 8)))

  (define (write-sfixed64 x op)
    (unless (int64? x)
      (errorf 'write-message "~s is not a sfixed64" x))
    (write-fixed 8 x op))

  ;; Length-delimited wire types

  (define-syntax wire-type-length-delimited (identifier-syntax 2))

  (define (read-string lip) (utf8->string (read-bytes lip)))

  (define (size-string x)
    (let lp ([i 0] [n (string-length x)] [size 0])
      (if (#3%fx= i n)
          size
          (lp (#3%fx+ i 1) n
            (let ([c (#3%char->integer (#3%string-ref x i))])
              (cond
               [(#3%fx<= c #x7f) (+ size 1)]
               [(#3%fx<= c #x7ff) (+ size 2)]
               [(#3%fx<= c #xffff) (+ size 3)]
               [else (+ size 4)]))))))

  (define (write-string x op)
    ;; We don't use string->utf8 to avoid allocating a bytevector.
    (write-varint (size-string x) op)
    (let lp ([i 0] [n (#3%string-length x)])
      (when (#3%fx< i n)
        (let ([c (#3%char->integer (#3%string-ref x i))])
          (cond
           [(#3%fx<= c #x7f)
            (put-u8 op c)]
           [(#3%fx<= c #x7ff)
            (put-u8 op (#3%fxior #b11000000 (#3%fxsrl c 6)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand c #b111111)))]
           [(#3%fx<= c #xffff)
            (put-u8 op (#3%fxior #b11100000 (#3%fxsrl c 12)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand (#3%fxsrl c 6) #b111111)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand c #b111111)))]
           [else
            (put-u8 op (#3%fxior #b11110000 (#3%fxsrl c 18)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand (#3%fxsrl c 12) #b111111)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand (#3%fxsrl c 6) #b111111)))
            (put-u8 op (#3%fxior #b10000000 (#3%fxlogand c #b111111)))]))
        (lp (#3%fx+ i 1) n))))

  (define (read-bv size lip)
    (update-limit! lip size)
    (let ([bv (get-bytevector-n (lip-ip lip) size)])
      (unless (and (bytevector? bv) (fx= (bytevector-length bv) size))
        (errorf 'read-message "unexpected eof"))
      bv))

  (define (read-bytes lip) (read-bv (read-varint lip) lip))

  (define-syntax size-bytes (identifier-syntax bytevector-length))

  (define (write-bytes x op)
    (write-varint (bytevector-length x) op)
    (put-bytevector op x))

  ;; 32-bit wire types

  (define-syntax wire-type-32-bit (identifier-syntax 5))

  (define (read-float lip)
    (bytevector-ieee-single-ref (read-bv 4 lip) 0 'little))

  (define-syntax size-float (identifier-syntax (lambda (x) 4)))

  (define (write-float x op)
    (let ([bv (make-bytevector 4)])
      (bytevector-ieee-single-set! bv 0 x 'little)
      (put-bytevector op bv)))

  (define (read-fixed32 lip) (read-fixed 4 lip))

  (define-syntax size-fixed32 (identifier-syntax (lambda (x) 4)))

  (define (write-fixed32 x op)
    (unless (uint32? x)
      (errorf 'write-message "~s is not a fixed32" x))
    (write-fixed 4 x op))

  (define (read-sfixed32 lip) (read-sfixed 4 lip))

  (define-syntax size-sfixed32 (identifier-syntax (lambda (x) 4)))

  (define (write-sfixed32 x op)
    (unless (int32? x)
      (errorf 'write-message "~s is not a sfixed32" x))
    (write-fixed 4 x op))
  )