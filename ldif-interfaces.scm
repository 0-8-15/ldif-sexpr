(define-interface
  ldif-rdn-constructor
  (
   ;; rdn distinguished names
   rdnsequence
   rdnsequence-empty?
   rdnsequence-cons
   rdnsequence-fold ;; ???
   kv-cons
   kv
   kv-empty
   kv?
   kv-k
   kv-v
   ))

(define-interface
  ldif-constructor
  (
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
