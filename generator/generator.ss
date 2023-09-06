;;; Copyright (c) 2023 Indigo BioAutomation, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software
;;; and associated documentation files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
;;; BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(library (generator generator)
  (export main)
  (import
   (chezscheme)
   (protobuf)
   (swish imports))

  (include "descriptor_pb.ss")
  (include "plugin_pb.ss")

  (define (main ip op)
    (write-message CodeGeneratorResponse (generate (read-message CodeGeneratorRequest ip)) op)
    (flush-output-port op))

  (define (generate req)
    (CodeGeneratorRequest open req (file_to_generate proto_file))
    (define fn->fdp (make-hashtable string-hash string=?)) ;; filename -> FileDescriptorProto
    (define fqn->desc (make-hashtable string-hash string=?)) ;; fully-qualified-name -> descriptor
    (define desc->qn (make-eq-hashtable)) ;; descriptor -> qualified-name
    (for-each
     (lambda (fdp)
       (FileDescriptorProto open fdp (name package message_type enum_type))
       (hashtable-set! fn->fdp name fdp)
       (register-messages! message_type package "" fqn->desc desc->qn)
       (register-enums! enum_type package "" fqn->desc desc->qn))
     proto_file)
    (make-message CodeGeneratorResponse
      [file (map (lambda (fn) (generate-file (hashtable-ref fn->fdp fn #f) fqn->desc desc->qn))
              file_to_generate)]))

  (define (get-type label type type_name fqn->desc desc->qn)
    (define repeated? (eqv? label (FieldDescriptorProto.Label LABEL_REPEATED)))
    (cond
     [(and repeated?
           (eqv? type (FieldDescriptorProto.Type TYPE_MESSAGE))
           (match (hashtable-ref fqn->desc type_name #f)
             [`(DescriptorProto
                [field ,(fields <= (`(FieldDescriptorProto [number 1])
                                    `(FieldDescriptorProto [number 2])))]
                [options `(MessageOptions [map_entry #t])])
              fields]
             [,_ #f]))
      => (lambda (fields)
           (define (inner-type field)
             (FieldDescriptorProto open field (label type type_name))
             (get-type label type type_name fqn->desc desc->qn))
           (match fields
             [(,key ,value) `(map ,(inner-type key) ,(inner-type value))]))]
     [repeated? `(repeated ,(get-base-type label type type_name fqn->desc desc->qn))]
     [else (get-base-type label type type_name fqn->desc desc->qn)]))

  (define (get-base-type label type type_name fqn->desc desc->qn)
    (cond
     [(eqv? type (FieldDescriptorProto.Type TYPE_DOUBLE)) 'double]
     [(eqv? type (FieldDescriptorProto.Type TYPE_FLOAT)) 'float]
     [(eqv? type (FieldDescriptorProto.Type TYPE_INT64)) 'int64]
     [(eqv? type (FieldDescriptorProto.Type TYPE_UINT64)) 'uint64]
     [(eqv? type (FieldDescriptorProto.Type TYPE_INT32)) 'int32]
     [(eqv? type (FieldDescriptorProto.Type TYPE_FIXED64)) 'fixed64]
     [(eqv? type (FieldDescriptorProto.Type TYPE_FIXED32)) 'fixed32]
     [(eqv? type (FieldDescriptorProto.Type TYPE_BOOL)) 'bool]
     [(eqv? type (FieldDescriptorProto.Type TYPE_STRING)) 'string]
     [(eqv? type (FieldDescriptorProto.Type TYPE_MESSAGE))
      `(message ,(fqn->qn type_name fqn->desc desc->qn))]
     [(eqv? type (FieldDescriptorProto.Type TYPE_BYTES)) 'bytes]
     [(eqv? type (FieldDescriptorProto.Type TYPE_UINT32)) 'uint32]
     [(eqv? type (FieldDescriptorProto.Type TYPE_ENUM))
      `(enum ,(fqn->qn type_name fqn->desc desc->qn))]
     [(eqv? type (FieldDescriptorProto.Type TYPE_SFIXED32)) 'sfixed32]
     [(eqv? type (FieldDescriptorProto.Type TYPE_SFIXED64)) 'sfixed64]
     [(eqv? type (FieldDescriptorProto.Type TYPE_SINT32)) 'sint32]
     [(eqv? type (FieldDescriptorProto.Type TYPE_SINT64)) 'sint64]
     [else #f]))

  (define (fqn->qn fqn fqn->desc desc->qn)
    (eq-hashtable-ref desc->qn (hashtable-ref fqn->desc fqn #f) fqn))

  (define (register-messages! dps package prefix fqn->desc desc->qn)
    (for-each
     (lambda (dp)
       (DescriptorProto open dp (name nested_type enum_type))
       (hashtable-set! fqn->desc (format ".~a.~a~a" package prefix name) dp)
       (let ([qn (string-append prefix name)])
         (eq-hashtable-set! desc->qn dp qn)
         (let ([prefix (string-append qn ".")])
           (register-messages! nested_type package prefix fqn->desc desc->qn)
           (register-enums! enum_type package prefix fqn->desc desc->qn))))
     dps))

  (define (register-enums! edps package prefix fqn->desc desc->qn)
    (for-each
     (lambda (edp)
       (EnumDescriptorProto open edp (name))
       (hashtable-set! fqn->desc (format ".~a.~a~a" package prefix name) edp)
       (eq-hashtable-set! desc->qn edp (string-append prefix name)))
     edps))

  (define (generate-file fdp fqn->desc desc->qn)
    (FileDescriptorProto open fdp (name dependency message_type enum_type))
    (let ([op (open-output-string)])
      (fprintf op ";;; Generated by the protocol buffer compiler. DO NOT EDIT!\n")
      (fprintf op ";;; source: ~a\n" name)
      (for-each (lambda (dep) (fprintf op ";;; include: ~a\n" dep)) dependency)
      (generate-messages message_type fqn->desc desc->qn op)
      (generate-enums enum_type desc->qn op)
      (make-message CodeGeneratorResponse.File
        [name (get-output-filename name)]
        [content (get-output-string op)])))

  (define (generate-messages dps fqn->desc desc->qn op)
    (for-each
     (lambda (dp)
       (DescriptorProto open dp (name field nested_type enum_type options))
       (unless (and options (MessageOptions map_entry options))
         (fprintf op "\n(define-message ~a" (eq-hashtable-ref desc->qn dp name))
         (for-each
          (lambda (fdp)
            (FieldDescriptorProto open fdp (name number label type type_name))
            (let ([type (get-type label type type_name fqn->desc desc->qn)])
              (when type
                (fprintf op "\n  (~a ~a ~a)" name type number))))
          field)
         (fprintf op ")\n")
         (generate-messages nested_type fqn->desc desc->qn op)
         (generate-enums enum_type desc->qn op)))
     dps))

  (define (generate-enums edps desc->qn op)
    (for-each
     (lambda (edp)
       (EnumDescriptorProto open edp (name value))
       (fprintf op "\n(define-enum ~a" (eq-hashtable-ref desc->qn edp name))
       (for-each
        (lambda (evdp)
          (EnumValueDescriptorProto open evdp (name number))
          (fprintf op "\n  (~a ~a)" name number))
        value)
       (fprintf op ")\n"))
     edps))

  (define (get-output-filename fn)
    (string-append (pregexp-replace (re "\\.[^.]*$") fn "") "_pb.ss")))
