;;(include "ldif-interfaces.scm")
(include "ldif-core.scm")
(include "rdn-as-sexpr.scm")

(module
 ldif-model-sexpr
 ((interface: ldif-constructor)
  )
 (import scheme)
 (cond-expand
  (chicken-4
   (use matchable)
   (import uri-common)
   (use (prefix base64 b64:))
   (import chicken)
   (import extras))
  (else
   (import (chicken blob))
   (import matchable)
   (import uri-common)
   (import (prefix base64 b64:))))
 (cond-expand
  (chicken-4 (import ports))
  (else (import (chicken port))))
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
   ;; Sadly the commented out version below is more appropriate.  To
   ;; ease the use (have less dots and blanks to type) I opted once
   ;; for the effective one.
   ;;
   ;; `((,k . ,v) . ,i)
   `((,k ,v) . ,i))

 (define (ldif-attributes-fold kons nil atts)
   (let loop ((i nil) (s atts))
     ;; Consequence of the ldif-attribute+ shortcomming we deconstruct differently.
     ;;
     ;; (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cdr a) i)) (cdr s)))
     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cadr a) i)) (cdr s)))))

 ) ;; end module ldif-model

;; Functor instanciation
(module ldif-sexpr = (ldif-core ldif-model-sexpr rdn-model-sexpr))
