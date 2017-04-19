;;(include "ldif-interfaces.scm")
(include "ldif-core.scm")
(include "rdn-as-sexpr.scm")
(require-library uri-common base64)
(module
 ldif-sexpr
 ((interface: ldif-constructor)
  )
 (import scheme matchable)
 (import uri-common)
 (import (prefix base64 b64:))
 (import chicken)

 (import extras)

 (define (base64-decode s) (string->blob (b64:base64-decode s)))
 (define (base64-encode b p) (b64:base64-encode (if (string? b) b (blob->string b)) p))

 (define (make-ldif name frame)
   `(LDIF ,name ,frame))

 (define (ldif? obj)
   (match obj (('LDIF name frame) #t) (_ #f)))

 (define (ldif-dn obj)
   (match obj (('LDIF name frame) name)))

 (define (ldif-attributes obj)
   (match obj (('LDIF name frame) frame)))

 (define (ldif-end) #!eof)

 (define (make-ldif-attdesc atttype options)
   (if (null? options) atttype (cons atttype options)))
 (define (ldif-attdesc? x)
   (or (string? x) (pair? x)))
 (define (ldif-attdesc-type x)
   (if (string? x) x (car x)))
 (define (ldif-attdesc-options x)
   (if (string? x) #f (cdr x)))

 (define (make-ldif-attribute-set) '())

 (define (ldif-attribute+ i k v)
   `((,k . ,v) . ,i))

 (define (ldif-attributes-fold kons nil atts)
   (let loop ((i nil) (s atts))
     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cdr a) i)) (cdr s)))))

 ) ;; end module ldif-model

;; Functor instanciation
(module ldif-as-sexpr = (ldif-core ldif-sexpr rdn-sexp))
