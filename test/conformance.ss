(library (test conformance)
  (export main)
  (import
   (chezscheme)
   (swish imports)
   (protobuf))

  (include "any_pb.ss")
  (include "conformance_pb.ss")
  (include "duration_pb.ss")
  (include "field_mask_pb.ss")
  (include "struct_pb.ss")
  (include "test_messages_proto2_pb.ss")
  (include "test_messages_proto3_pb.ss")
  (include "timestamp_pb.ss")
  (include "wrappers_pb.ss")

  (define default-response
    (ConformanceResponse make
      [parse_error ""]
      [serialize_error ""]
      [runtime_error ""]
      [protobuf_payload '#vu8()]
      [json_payload ""]
      [skipped ""]
      [jspb_payload ""]
      [text_payload ""]))

  (define (read-request ip)
    (let ([bv (get-bytevector-n ip 4)])
      (cond
       [(eof-object? bv) #f]
       [(eqv? (bytevector-length bv) 4)
        (read-message ConformanceRequest ip
          (bytevector-u32-ref bv 0 'little))]
       [else (raise 'unexpected-eof)])))

  (define (write-response x op)
    (write-message ConformanceResponse x op
      (lambda (size)
        (let ([bv (make-bytevector 4)])
          (bytevector-u32-set! bv 0 size 'little)
          (put-bytevector op bv))))
    (flush-output-port op))

  (define (main ip op)
    (match (read-request ip)
      [#f #t]
      [`(ConformanceRequest ,protobuf_payload ,requested_output_format ,message_type)
       (cond
        [(zero? (bytevector-length protobuf_payload))
         (write-response
          (ConformanceResponse copy default-response
            [skipped "no protobuf_payload"])
          op)]
        [(not (eqv? requested_output_format (WireFormat PROTOBUF)))
         (write-response
          (ConformanceResponse copy default-response
            [skipped "requested_output_format != PROTOBUF"])
          op)]
        [(string=? message_type "protobuf_test_messages.proto2.TestAllTypesProto2")
         (match (catch (read-message TestAllTypesProto2 protobuf_payload))
           [#(EXIT ,reason)
            (write-response
             (ConformanceResponse copy default-response
               [parse_error (exit-reason->english reason)])
             op)]
           [,msg
            (match (catch (write-message TestAllTypesProto2 msg))
              [#(EXIT ,reason)
               (write-response
                (ConformanceResponse copy default-response
                  [serialize_error (exit-reason->english reason)])
                op)]
              [,msg
               (write-response
                (ConformanceResponse copy default-response
                  [protobuf_payload msg])
                op)])])]
        [(string=? message_type "protobuf_test_messages.proto3.TestAllTypesProto3")
         (match (catch (read-message TestAllTypesProto3 protobuf_payload))
           [#(EXIT ,reason)
            (write-response
             (ConformanceResponse copy default-response
               [parse_error (exit-reason->english reason)])
             op)]
           [,msg
            (match (catch (write-message TestAllTypesProto3 msg))
              [#(EXIT ,reason)
               (write-response
                (ConformanceResponse copy default-response
                  [serialize_error (exit-reason->english reason)])
                op)]
              [,msg
               (write-response
                (ConformanceResponse copy default-response
                  [protobuf_payload msg])
                op)])])]
        [else
         (write-response
          (ConformanceResponse copy default-response
            [skipped "unsupported message_type"])
          op)])
       (main ip op)])))
