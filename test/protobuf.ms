(import
 (chezscheme)
 (protobuf)
 (swish imports)
 (swish mat)
 )

(include "test_messages_proto2_pb.ss")
(include "test_options_pb.ss")

(define-syntax (assert-equal? x)
  (syntax-case x ()
    [(_ e1 e2)
     #`(let ([a e1] [b e2])
         (unless (equal? a b)
           (errorf #f "~s expected ~s, received ~s" #,(find-source x) a b)))]))

(define (make-varint-list start limit)
  (let lp ([bits start])
    (if (>= bits limit)
        '()
        (list* (- (ash 1 bits) 1)
          (ash 1 bits)
          (+ (ash 1 bits) 1 (random (- (ash 1 bits) 1)))
          (lp (+ bits 7))))))

(define int32-list
  (list* 0 -1 (- (ash 1 31) 1) (- (ash 1 31)) (make-varint-list 7 31)))

(define sint32-list
  (let ([base (list* 1 2 (make-varint-list 6 30))])
    (cons 0 (append (map - base) base))))

(define uint32-list
  (list* 0 (- (ash 1 32) 1) (make-varint-list 7 32)))

(define int64-list
  (list* 0 -1 (- (ash 1 63) 1) (- (ash 1 63)) (make-varint-list 7 63)))

(define sint64-list
  (let ([base (list* 1 2 (make-varint-list 6 62))])
    (cons 0 (append (map - base) base))))

(define uint64-list
  (list* 0 (- (ash 1 64) 1) (make-varint-list 7 64)))

(define all-bytes
  (let ([bv (make-bytevector 256)])
    (do ([i 0 (+ i 1)]) [(eqv? i 256)]
      (bytevector-u8-set! bv i i))
    bv))

(define (bv-append bv1 bv2)
  (call-with-bytevector-output-port
   (lambda (op)
     (put-bytevector op bv1)
     (put-bytevector op bv2))))

(define (bv-repeat bv n)
  (call-with-bytevector-output-port
   (lambda (op)
     (do ([i 0 (+ i 1)]) ((= i n))
       (put-bytevector op bv)))))

(define (make-msg depth)
  (make-message TestAllTypesProto2
    [optional_int32 #x-76543210]
    [optional_int64 #x-7654321089ABCDEF]
    [optional_uint32 #xFEDCBA98]
    [optional_uint64 #xFEDCBA9876543210]
    [optional_sint32 #x-76543210]
    [optional_sint64 #x-7654321089ABCDEF]
    [optional_fixed32 #xFEDCBA98]
    [optional_fixed64 #xFEDCBA9876543210]
    [optional_sfixed32 #x-76543210]
    [optional_sfixed64 #x-76543210]
    [optional_float 3.1415927410125732]
    [optional_double (* (asin 1) 2)]
    [optional_bool #t]
    [optional_string "\x0;\x1;\x7F;\x80;\x81;\x7FF;\x800;\x801;\xFFFF;\x10000;\x10001;\x10FFFF;"]
    [optional_bytes all-bytes]
    [optional_nested_message
     (and (< depth 3)
          (TestAllTypesProto2.NestedMessage make
            [a depth]
            [corecursive (and (eqv? depth 1) (make-msg 2))]))]
    [optional_foreign_message (ForeignMessageProto2 make [c #x76543210])]
    [optional_nested_enum (TestAllTypesProto2.NestedEnum NEG)]
    [optional_foreign_enum (ForeignEnumProto2 FOREIGN_BAR)]
    [optional_string_piece "\x3D;\x527;\xA8B6;\x101CC5;"]
    [recursive_message (and (< depth 3) (make-msg (+ depth 1)))]
    [repeated_int32 int32-list]
    [repeated_int64 int64-list]
    [repeated_uint32 uint32-list]
    [repeated_uint64 uint64-list]
    [repeated_sint32 sint32-list]
    [repeated_sint64 sint64-list]
    [repeated_fixed32 '(0 #xFEDCBA98)]
    [repeated_fixed64 '(0 #xFEDCBA9876543210)]
    [repeated_sfixed32 '(0 #x-76543210)]
    [repeated_sfixed64 '(0 #x-7654321089ABCDEF)]
    [repeated_float '(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
    [repeated_double '(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
    [repeated_bool '(#f #t)]
    [repeated_string '("\x25CF;bullet" "" "\x10000;huge")]
    [repeated_bytes '(#vu8() #vu8(1 2))]
    [repeated_nested_message
     (list
      (TestAllTypesProto2.NestedMessage make
        [a depth]
        [corecursive (and (eqv? depth 1) (make-msg 2))])
      (TestAllTypesProto2.NestedMessage make
        [a 0]
        [corecursive #f]))]
    [repeated_foreign_message
     (list
      (ForeignMessageProto2 make [c 0])
      (ForeignMessageProto2 make [c 1])
      (ForeignMessageProto2 make [c -1]))]
    [repeated_nested_enum
     (list
      (TestAllTypesProto2.NestedEnum FOO)
      (TestAllTypesProto2.NestedEnum BAR)
      (TestAllTypesProto2.NestedEnum BAZ)
      (TestAllTypesProto2.NestedEnum NEG))]
    [repeated_foreign_enum
     (list
      (ForeignEnumProto2 FOREIGN_FOO)
      (ForeignEnumProto2 FOREIGN_BAR)
      (ForeignEnumProto2 FOREIGN_BAZ))]
    [map_int32_int32 (map cons int32-list int32-list)]
    [map_int64_int64 (map cons int64-list int64-list)]
    [map_uint32_uint32 (map cons uint32-list uint32-list)]
    [map_uint64_uint64 (map cons uint64-list uint64-list)]
    [map_sint32_sint32 (map cons sint32-list sint32-list)]
    [map_sint64_sint64 (map cons sint64-list sint64-list)]
    [map_fixed32_fixed32 '((0 . 0) (#xFEDCBA98 . #xFEDCBA98))]
    [map_fixed64_fixed64 '((0 . 0) (#xFEDCBA9876543210 . #xFEDCBA9876543210))]
    [map_sfixed32_sfixed32 '((0 . 0) (#x-76543210 . #x-76543210))]
    [map_sfixed64_sfixed64 '((0 . 0) (#x-7654321089ABCDEF . #x-7654321089ABCDEF))]
    [map_int32_float '((0 . 0.0) (1 . 1.0) (2 . 2.0) (-1 . -1.0))]
    [map_int32_double '((0 . 0.0) (1 . 1.0) (2 . 2.0) (-1 . -1.0))]
    [map_bool_bool '((#f . #f) (#f . #t) (#t . #f) (#t . #t))]
    [map_string_string '(("" . "a") ("a" . "bb") ("bb" . "ccc"))]
    [map_string_bytes '(("" . #vu8(1 2)) ("a" . #vu8(3 4)))]
    [map_string_nested_message
     `(("" . ,(TestAllTypesProto2.NestedMessage make
                [a 0]
                [corecursive #f]))
       ("a" . ,(TestAllTypesProto2.NestedMessage make
                 [a 2]
                 [corecursive #f])))]
    [map_string_foreign_message
     `(("" . ,(ForeignMessageProto2 make [c 0]))
       ("1" . ,(ForeignMessageProto2 make [c 1]))
       ("-1" . ,(ForeignMessageProto2 make [c -1])))]
    [map_string_nested_enum
     `(("" . ,(TestAllTypesProto2.NestedEnum FOO))
       ("a" . ,(TestAllTypesProto2.NestedEnum BAR))
       ("b" . ,(TestAllTypesProto2.NestedEnum BAZ))
       ("c" . ,(TestAllTypesProto2.NestedEnum NEG)))]
    [map_string_foreign_enum
     `(("" . ,(ForeignEnumProto2 FOREIGN_FOO))
       ("a" . ,(ForeignEnumProto2 FOREIGN_BAR))
       ("b" . ,(ForeignEnumProto2 FOREIGN_BAZ)))]
    [fieldname1 (- (ash 1 7) 1)]
    [field_name2 (ash 1 7)]
    [_field_name3 (+ (ash 1 7) 1)]
    [field__name4_ (- (ash 1 14) 1)]
    [field0name5 (ash 1 14)]
    [field_0_name6 (+ (ash 1 14) 1)]
    [fieldName7 (- (ash 1 21) 1)]
    [FieldName8 (ash 1 21)]
    [field_Name9 (+ (ash 1 21) 1)]
    [Field_Name10 (- (ash 1 28) 1)]
    [FIELD_NAME11 (ash 1 28)]
    [FIELD_name12 (+ (ash 1 28) 1)]
    [__field_name13 (- (ash 1 31) 1)]
    [__Field_name14 -1]
    [field__name15 -2]
    [field__Name16 (- (ash 1 31))]
    [field_name17__ (- (ash 1 7))]
    [Field_name18__ (- (ash 1 14))]))

(mat round-trip ()
  (let* ([m1 (make-msg 1)]
         [m2 (read-message TestAllTypesProto2
               (write-message TestAllTypesProto2 m1))])
    (assert-equal? m1 m2)))

(define-syntax error-text
  (syntax-rules ()
    [(_ e) (exit-reason->english (catch e))]))

(define-syntax define-bad-repeated
  (syntax-rules ()
    [(_ bad msg-type repeat)
     (define-syntax bad
       (syntax-rules ()
         [(_ type field e)
          (let* ([v e]
                 [m (error-text
                     (write-message msg-type
                       (make-message msg-type [field (repeat v)])))])
            (string=? m
              (format "Exception in ~a.~a: ~s is not a ~a."
                'msg-type 'field v 'type)))]))]))

(define-syntax define-helpers
  (syntax-rules ()
    [(_ msg-type bad-field bad-key bad-value)
     (begin
       (define-syntax bad-field
         (syntax-rules ()
           [(_ type field e)
            (let* ([v e]
                   [m (error-text
                       (write-message msg-type
                         (make-message msg-type [field v])))])
              (string=? m
                (format "Exception in ~a.~a: ~s is not a ~a."
                  'msg-type 'field v 'type)))]))
       (define-syntax bad-key
         (syntax-rules ()
           [(_ type field $key $value)
            (let* ([key $key] [value $value]
                   [m (error-text
                       (write-message msg-type
                         (make-message msg-type
                           [field (list (cons key value))])))])
              (string=? m
                (format "Exception in ~a.~a: ~s is not a ~a."
                  'msg-type 'field key 'type)))]))
       (define-syntax bad-value
         (syntax-rules ()
           [(_ type field $key $value)
            (let* ([key $key] [value $value]
                   [m (error-text
                       (write-message msg-type
                         (make-message msg-type
                           [field (list (cons key value))])))])
              (string=? m
                (format "Exception in ~a.~a: ~s is not a ~a."
                  'msg-type 'field value 'type)))])))]))

(mat write-errors ()
  (define-helpers TestAllTypesProto2 bad-field bad-key bad-value)
  (define-bad-repeated bad-list TestAllTypesProto2 list)

  (assert (bad-field int32 optional_int32 1.5))
  (assert (bad-field int64 optional_int64 1/2))
  (assert (bad-field uint32 optional_uint32 -1))
  (assert (bad-field uint64 optional_uint64 #f))
  (assert (bad-field sint32 optional_sint32 "y"))
  (assert (bad-field sint64 optional_sint64 '()))
  (assert (bad-field fixed32 optional_fixed32 -1))
  (assert (bad-field fixed64 optional_fixed64 -1))
  (assert (bad-field sfixed32 optional_sfixed32 3/2))
  (assert (bad-field sfixed64 optional_sfixed64 +inf.0))
  (assert (bad-field float optional_float #t))
  (assert (bad-field double optional_double "x"))
  (assert (bad-field string optional_string #f))
  (assert (bad-field bytevector optional_bytes "x"))
  (assert (bad-field ForeignMessageProto2 optional_foreign_message 'foreign))
  (assert (bad-field ForeignEnumProto2 optional_foreign_enum 'enum))

  (assert (bad-field list repeated_int32 1))
  (assert (bad-list int32 repeated_int32 1.5))
  (assert (bad-list int64 repeated_int64 1/2))
  (assert (bad-list uint32 repeated_uint32 -1))
  (assert (bad-list uint64 repeated_uint64 #f))
  (assert (bad-list sint32 repeated_sint32 "y"))
  (assert (bad-list sint64 repeated_sint64 '()))
  (assert (bad-list fixed32 repeated_fixed32 -1))
  (assert (bad-list fixed64 repeated_fixed64 -1))
  (assert (bad-list sfixed32 repeated_sfixed32 3/2))
  (assert (bad-list sfixed64 repeated_sfixed64 +inf.0))
  (assert (bad-list float repeated_float #t))
  (assert (bad-list double repeated_double "x"))
  (assert (bad-list string repeated_string #f))
  (assert (bad-list bytevector repeated_bytes "x"))
  (assert (bad-list ForeignMessageProto2 repeated_foreign_message 'foreign))
  (assert (bad-list ForeignEnumProto2 repeated_foreign_enum 'enum))

  (assert (bad-field map map_int32_int32 1))
  (assert (bad-field map map_int32_int32 '(1 2)))
  (assert (bad-key int32 map_int32_int32 1.5 0))
  (assert (bad-key int64 map_int64_int64 1/2 0))
  (assert (bad-key uint32 map_uint32_uint32 -1 0))
  (assert (bad-key uint64 map_uint64_uint64 #f 0))
  (assert (bad-key sint32 map_sint32_sint32 "y" 0))
  (assert (bad-key sint64 map_sint64_sint64 '() 0))
  (assert (bad-key fixed32 map_fixed32_fixed32 -1 0))
  (assert (bad-key fixed64 map_fixed64_fixed64 -1 0))
  (assert (bad-key sfixed32 map_sfixed32_sfixed32 3/2 0))
  (assert (bad-key sfixed64 map_sfixed64_sfixed64 +inf.0 0))
  (assert (bad-key string map_string_string 'x "y"))

  (assert (bad-value int32 map_int32_int32 0 1.5))
  (assert (bad-value int64 map_int64_int64 0 1/2))
  (assert (bad-value uint32 map_uint32_uint32 0 -1))
  (assert (bad-value uint64 map_uint64_uint64 0 #f))
  (assert (bad-value sint32 map_sint32_sint32 0 "y"))
  (assert (bad-value sint64 map_sint64_sint64 0 '()))
  (assert (bad-value fixed32 map_fixed32_fixed32 0 -1))
  (assert (bad-value fixed64 map_fixed64_fixed64 0 -1))
  (assert (bad-value sfixed32 map_sfixed32_sfixed32 0 3/2))
  (assert (bad-value sfixed64 map_sfixed64_sfixed64 0 +inf.0))
  (assert (bad-value float map_int32_float 0 #t))
  (assert (bad-value double map_int32_double 0 "x"))
  (assert (bad-value string map_string_string "x" 'y))
  (assert (bad-value bytevector map_string_bytes "x" "y"))
  (assert (bad-value ForeignMessageProto2 map_string_foreign_message "x" 'y))
  (assert (bad-value ForeignEnumProto2 map_string_foreign_enum "x" #f)))

(mat swish-options ()
  (define-helpers SpecialTypesMessage bad-field bad-key bad-value)
  (define-bad-repeated bad-list SpecialTypesMessage list)
  (define-bad-repeated bad-vector SpecialTypesMessage vector)

  ;; round trip
  (let* ([m1 (make-message SpecialTypesMessage
               [repeated_double_flvector '#vfl(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
               [repeated_double_list '(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
               [repeated_double_vector '#(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
               [repeated_float_flvector '#vfl(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)]
               [repeated_float_vector '#(0.0 1.0 2.0 4.0 -4.0 -2.0 -1.0)])]
         [m2 (read-message SpecialTypesMessage
               (write-message SpecialTypesMessage m1))])
    (assert-equal? m1 m2))
  ;; optional/empty values
  (let* ([m1 (make-message SpecialTypesMessage)]
         [m2 (read-message SpecialTypesMessage
               (write-message SpecialTypesMessage m1))])
    (assert-equal? m1 m2))
  ;; write errors
  (assert (bad-field flvector repeated_double_flvector "x"))
  (assert (bad-field flvector repeated_float_flvector "x"))
  (assert (bad-field vector repeated_double_vector "x"))
  (assert (bad-field vector repeated_float_vector "x"))
  (assert (bad-list double repeated_double_list "x"))
  (assert (bad-vector double repeated_double_vector "x"))
  (assert (bad-vector float repeated_float_vector "x"))
  )

(mat lazy-flvector ()
  ;; This provides coverage of consolidate-lazy-flvector. The test
  ;; scribbles individual flvectors, then takes the resulting
  ;; bytevectors and appends them. Reading these messages triggers the
  ;; cases where the input is a set of pairs containing flvectors.
  (define (pickle vfl)
    (write-message FlvectorValue (make-message FlvectorValue [value vfl])))
  (define (unpickle bv)
    (FlvectorValue value (read-message FlvectorValue bv)))
  (let ([bv (bv-append (pickle '#vfl(1.0)) (pickle '#vfl(2.0 3.0)))])
    (assert-equal? '#vfl(1.0 2.0 3.0)
      (unpickle bv))
    (assert-equal? '#vfl(1.0 2.0 3.0 1.0 2.0 3.0)
      (unpickle (bv-repeat bv 2)))
    (assert-equal? '#vfl(1.0 2.0 3.0 1.0 2.0 3.0 1.0 2.0 3.0)
      (unpickle (bv-repeat bv 3))))
  )

(mat merge-flvector ()
  ;; This provides coverage of merge-field with flvector types.
  (define (pickle vfl)
    (write-message Container
      (make-message Container
        [inner (make-message FlvectorValue [value vfl])])))
  (define (unpickle bv)
    (FlvectorValue value (Container inner (read-message Container bv))))
  (let ([bv (bv-append (pickle '#vfl(1.0)) (pickle '#vfl(2.0 3.0)))])
    (assert-equal? '#vfl(1.0 2.0 3.0)
      (unpickle bv))
    (assert-equal? '#vfl(1.0 2.0 3.0 1.0 2.0 3.0)
      (unpickle (bv-repeat bv 2)))
    (assert-equal? '#vfl(1.0 2.0 3.0 1.0 2.0 3.0 1.0 2.0 3.0)
      (unpickle (bv-repeat bv 3))))
  )
