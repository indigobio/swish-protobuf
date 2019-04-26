(library (test smoke)
  (export make-msg smoke-test)
  (import
   (chezscheme)
   (swish imports)
   (protobuf))

  (include "test_messages_proto2_pb.ss")

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

  (define (make-msg depth)
    (TestAllTypesProto2 make
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
      [optional_cord ""]
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
      [repeated_string_piece '()]
      [repeated_cord '()]
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
      [oneof_uint32 0]
      [oneof_nested_message #f]
      [oneof_string ""]
      [oneof_bytes '#vu8()]
      [oneof_bool #f]
      [oneof_uint64 0]
      [oneof_float 0.0]
      [oneof_double 0.0]
      [oneof_enum 0]
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

  (define (smoke-test)
    (let* ([m1 (make-msg 1)]
           [m2 (read-message TestAllTypesProto2
                 (write-message TestAllTypesProto2 m1))])
      (if (equal? m1 m2)
          (let ([op (console-error-port)])
            (fprintf op "Smoke test passed!\n")
            (flush-output-port op))
          (errorf 'smoke-test "~s != ~s" m1 m2))))
  )
