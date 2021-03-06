;; This is a work around to avoid having to have so many import
;; libraries for all the sum modules.  We simply copy the stuff in here.

(cond-expand
 (chicken-4
;; Trying to get around the work around
(require-extension uri-common base64)
(module
 ldif-sexpr
 (
  ;; # 1. Inline replacement of imported interface definition.
  ;;
  ;; ## 1.1 rdn distinguished names - Including manually added prefix `rfc4514-`
   rfc4514-rdnsequence
   rfc4514-rdnsequence-empty?
   rfc4514-rdnsequence-cons
   rfc4514-rdnsequence-fold ;; ???
   rfc4514-kv-cons
   rfc4514-kv
   rfc4514-kv-empty
   rfc4514-kv?
   rfc4514-kv-k
   rfc4514-kv-v
  ;; ## 1.2 ldif object constructors
   ;; Root object
   make-ldif
   ldif?
   ldif-dn ldif-attributes
   ldif-end ;; end iteration
   ;; single ldif attribute
   make-ldif-attdesc
   ldif-attdesc?
   ldif-attdesc-type
   ldif-attdesc-options
   ;; ldif attribute set
   make-ldif-attribute-set
   ldif-attribute+
   ldif-attributes-fold
  ;;
;; FIXME  ldif-ref ;; return the ref URL if the LDIF record is actually a `ref:`
  ;;
  ;; RFC 2849
  ;;
  ;; TBD: Does NOT yet parse `ldif-change-record`'s, only `ldif-attrval-record`'s.

  ;; generic part (the functor export)
  read write
  rfc4514-read rfc4514-write
  ;; RFC 2254 "String Representation of LDAP Search Filters"
  write-ldap-filter ldap-filter-string
  )

 ;; # 2 Inline replacement of datatype implementations
 (import (except scheme read write))
 ;; ## 2.1 RDN - Including manually added prefix `rfc4514-`
 (define (rfc4514-rdnsequence . args) args)
 (define (rfc4514-rdnsequence-cons a b) (cons a b))
 (define rfc4514-rdnsequence-empty? null?)
 (define (rfc4514-rdnsequence-fold kons nil s)
   (let loop ((i nil) (s s))
     (if (null? s) i (loop (kons (car s) i) (cdr s)))))
 (define (rfc4514-kv-empty) '())
 (define (rfc4514-kv-cons a b) (cons a b))
 (define rfc4514-kv? pair?)
 (define (rfc4514-kv k v) (list k v))
 (define (rfc4514-kv-k x) (car x))
 (define (rfc4514-kv-v x) (cadr x))
 ;; ## 2.2 LDIF
 (import chicken matchable)
 (import uri-common)
 (import (prefix base64 b64:))

 (define-record ldif dn attributes)

 (define (ldif-end) #!eof)

 ;; ## 2.2.2
 (define (make-ldif-attdesc atttype options)
   (if (null? options) atttype (cons atttype options)))
 (define (ldif-attdesc? x)
   (or (string? x) (pair? x)))
 (define (ldif-attdesc-type x)
   (if (string? x) x (car x)))
 (define (ldif-attdesc-options x)
   (if (string? x) #f (cdr x)))

 (define (make-ldif-attribute-set) '())

 ;; (define (ldif-attribute+ i k v) `((,k . ,v) . ,i))

 (define (ldif-attribute+ i k v)
   `((,k ,v) . ,i))

 ;; (define (ldif-attributes-fold kons nil atts)
 ;;   (let loop ((i nil) (s atts))
 ;;     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cdr a) i)) (cdr s)))))

 (define (ldif-attributes-fold kons nil atts)
   (let loop ((i nil) (s atts))
     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cadr a) i)) (cdr s)))))

 ;; ## 2.2.3
 (define (base64-decode s) (string->blob (b64:base64-decode s)))
 (define (base64-encode b p) (b64:base64-encode (if (string? b) b (blob->string b)) p))

 ;; The (replacement of the) actual functor instantiation.

 (cond-expand
  (chicken-4
   (import (prefix scheme s:))
   (import (except scheme read write))
   (import chicken)
   (require-extension extras)
   (import (only extras read-line))
   (import (only data-structures identity string-split))
   (use srfi-1 srfi-13 srfi-14 irregex))
  (else
   (import (chicken base))
   (import (chicken blob))
   (import (chicken fixnum))
   (import (chicken irregex))
   (import (only (chicken io) read-line))
   (import (only (chicken string) string-split))
   (import srfi-1 srfi-13 srfi-14)
   (import matchable uri-common)))

 (cond-expand
  (chicken-4 (import ports))
  (else (import (chicken port))))
 (include "ldif-core-impl.scm")

 )
) ;; end cond-expand for chicken-4
(else
 (module
  ldif-core
  (
   make-ldif-exports
   )
  (import (except scheme read write))
  (cond-expand
   (chicken-5
    (import (chicken base))
    (import (chicken blob))
    (import (chicken fixnum))
    (import (chicken irregex))
    (import (only (chicken io) read-line))
    (import (only (chicken string) string-split))
    (import srfi-1 srfi-13 srfi-14)
    (import matchable uri-common)
    (import (chicken port))))
  (define (make-ldif-exports LDIF RDN)
    (define-values
     (;; LDIF constructors and accessors
      ;; Root object
      make-ldif
      ldif?
      ldif-dn ldif-attributes
      ldif-end ;; end iteration
      ;; single ldif attribute
      make-ldif-attdesc
      ldif-attdesc?
      ldif-attdesc-type
      ldif-attdesc-options
      ;; ldif attribute set
      make-ldif-attribute-set
      ldif-attribute+
      ldif-attributes-fold
      ;; uri attribute values
      uri-reference? uri-reference uri->string
      ;; base64
      base64-decode base64-encode
      ) (LDIF))
    (define-values
      (;; rdn distinguished names
       rfc4514-rdnsequence
       rfc4514-rdnsequence-empty?
       rfc4514-rdnsequence-cons
       rfc4514-rdnsequence-fold ;; ???
       rfc4514-kv-cons
       rfc4514-kv
       rfc4514-kv-empty
       rfc4514-kv?
       rfc4514-kv-k
       rfc4514-kv-v
       ) (RDN))
    (include "ldif-core-impl.scm")
    (values
     ;; RFC 2849
     ;;
     ;; TBD: Does NOT yet parse `ldif-change-record`'s, only `ldif-attrval-record`'s.
     read write
     rfc4514-read rfc4514-write
     ;; RFC 2254 "String Representation of LDAP Search Filters"
     write-ldap-filter ldap-filter-string
     )))
 (module
  ldif-sexpr
  (
   ;; # 1. Inline replacement of imported interface definition.
   ;;
   ;; ## 1.1 rdn distinguished names - Including manually added prefix `rfc4514-`
   rfc4514-rdnsequence
   rfc4514-rdnsequence-empty?
   rfc4514-rdnsequence-cons
   rfc4514-rdnsequence-fold ;; ???
   rfc4514-kv-cons
   rfc4514-kv
   rfc4514-kv-empty
   rfc4514-kv?
   rfc4514-kv-k
   rfc4514-kv-v
   ;; ## 1.2 ldif object constructors
   ;; Root object
   make-ldif
   ldif?
   ldif-dn ldif-attributes
   ldif-end ;; end iteration
   ;; single ldif attribute
   make-ldif-attdesc
   ldif-attdesc?
   ldif-attdesc-type
   ldif-attdesc-options
   ;; ldif attribute set
   make-ldif-attribute-set
   ldif-attribute+
   ldif-attributes-fold
   ;;
;;FIXME   ldif-ref ;; return the ref URL if the LDIF record is actually a `ref:`
   ;;
   ;; RFC 2849
   ;;
   ;; TBD: Does NOT yet parse `ldif-change-record`'s, only `ldif-attrval-record`'s.

   ;; generic part (the functor export)
   read write
   rfc4514-read rfc4514-write
   ;; RFC 2254 "String Representation of LDAP Search Filters"
   write-ldap-filter ldap-filter-string
   )
  (import (except scheme read write))
  (import (chicken base))
  (import (chicken blob))
  (import (only uri-common uri->string uri-reference uri-reference?))
  (import (prefix base64 b64:))
  ;; ## 2.1 RDN - Including manually added prefix `rfc4514-`
  (define (rfc4514-rdnsequence . args) args)
  (define (rfc4514-rdnsequence-cons a b) (cons a b))
  (define rfc4514-rdnsequence-empty? null?)
  (define (rfc4514-rdnsequence-fold kons nil s)
    (let loop ((i nil) (s s))
      (if (null? s) i (loop (kons (car s) i) (cdr s)))))
  (define (rfc4514-kv-empty) '())
  (define (rfc4514-kv-cons a b) (cons a b))
  (define rfc4514-kv? pair?)
  (define (rfc4514-kv k v) (list k v))
  (define (rfc4514-kv-k x) (car x))
  (define (rfc4514-kv-v x) (cadr x))
  (define-record ldif dn attributes)

  (define (ldif-end) #!eof)

  ;; ## 2.2.2
  (define (make-ldif-attdesc atttype options)
    (if (null? options) atttype (cons atttype options)))
  (define (ldif-attdesc? x)
    (or (string? x) (pair? x)))
  (define (ldif-attdesc-type x)
    (if (string? x) x (car x)))
  (define (ldif-attdesc-options x)
    (if (string? x) #f (cdr x)))

  (define (make-ldif-attribute-set) '())

  ;; (define (ldif-attribute+ i k v) `((,k . ,v) . ,i))

  (define (ldif-attribute+ i k v)
    `((,k ,v) . ,i))

  ;; (define (ldif-attributes-fold kons nil atts)
  ;;   (let loop ((i nil) (s atts))
  ;;     (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cdr a) i)) (cdr s)))))

  (define (ldif-attributes-fold kons nil atts)
    (let loop ((i nil) (s atts))
      (if (null? s) i (loop (let ((a (car s))) (kons (car a) (cadr a) i)) (cdr s)))))

  ;; ## 2.2.3
  (define (base64-decode s) (string->blob (b64:base64-decode s)))
  (define (base64-encode b p) (b64:base64-encode (if (string? b) b (blob->string b)) p))

  (import ldif-core)
  (define (LDIF)
    (values
     ;; Root object
     make-ldif
     ldif?
     ldif-dn ldif-attributes
     ldif-end ;; end iteration
     ;; single ldif attribute
     make-ldif-attdesc
     ldif-attdesc?
     ldif-attdesc-type
     ldif-attdesc-options
     ;; ldif attribute set
     make-ldif-attribute-set
     ldif-attribute+
     ldif-attributes-fold
     ;; uri attribute values
     uri-reference? uri-reference uri->string
     ;; base64
     base64-decode base64-encode
     ))
  (define (RDN)
    (values
     ;; rdn distinguished names
     rfc4514-rdnsequence
     rfc4514-rdnsequence-empty?
     rfc4514-rdnsequence-cons
     rfc4514-rdnsequence-fold ;; ???
     rfc4514-kv-cons
     rfc4514-kv
     rfc4514-kv-empty
     rfc4514-kv?
     rfc4514-kv-k
     rfc4514-kv-v
     ))
  (define-values
    (read
     write
     rfc4514-read
     rfc4514-write
     ;; RFC 2254 "String Representation of LDAP Search Filters"
     write-ldap-filter
     ldap-filter-string
     )
    (make-ldif-exports LDIF RDN))

  ) ;; end ldif-sexpr
 ) ;; end cond-expand for chicken-5
)
