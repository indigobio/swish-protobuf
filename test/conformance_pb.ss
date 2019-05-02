;;; Generated by the protocol buffer compiler. DO NOT EDIT!
;;; source: conformance.proto

(define-message FailureSet
  (failure (repeated string) 1))

(define-message ConformanceRequest
  (protobuf_payload bytes 1)
  (json_payload string 2)
  (jspb_payload string 7)
  (text_payload string 8)
  (requested_output_format (enum WireFormat) 3)
  (message_type string 4)
  (test_category (enum TestCategory) 5)
  (jspb_encoding_options (message JspbEncodingConfig) 6)
  (print_unknown_fields bool 9))

(define-message ConformanceResponse
  (parse_error string 1)
  (serialize_error string 6)
  (runtime_error string 2)
  (protobuf_payload bytes 3)
  (json_payload string 4)
  (skipped string 5)
  (jspb_payload string 7)
  (text_payload string 8))

(define-message JspbEncodingConfig
  (use_jspb_array_any_format bool 1))

(define-enum WireFormat
  (UNSPECIFIED 0)
  (PROTOBUF 1)
  (JSON 2)
  (JSPB 3)
  (TEXT_FORMAT 4))

(define-enum TestCategory
  (UNSPECIFIED_TEST 0)
  (BINARY_TEST 1)
  (JSON_TEST 2)
  (JSON_IGNORE_UNKNOWN_PARSING_TEST 3)
  (JSPB_TEST 4)
  (TEXT_FORMAT_TEST 5))