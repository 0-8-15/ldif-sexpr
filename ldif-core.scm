(include "ldif-interfaces.scm")
(functor
 (ldif-core (LDIF ldif-constructor) (RDN ldif-rdn-constructor))
 (;; RFC 2849
  ;;
  ;; TBD: Does NOT yet parse `ldif-change-record`'s, only `ldif-attrval-record`'s.
  read write
  rfc4514-read rfc4514-write
  ;; RFC 2254 "String Representation of LDAP Search Filters"
  write-ldap-filter ldap-filter-string
  )
 (import LDIF)
 (import (prefix RDN rfc4514-))
 (import (prefix scheme s:))
 (import (except scheme read write))
 (cond-expand
  (chicken-4
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

 (include "ldif-core-impl.scm")
 )
