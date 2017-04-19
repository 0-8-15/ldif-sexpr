(include "ldif-interfaces.scm")
(functor
 (ldif-core (LDIF ldif-constructor) (RDN ldif-rdn-constructor))
 (;; RFC 2849
  ;;
  ;; TBD: Does NOT yet parse `ldif-change-record`'s, only `ldif-attrval-record`'s.
  read write
  rfc4514-read rfc4514-write
  )
 (import LDIF)
 (import (prefix RDN rfc4514-))
 (import (prefix scheme s:))
 (import (except scheme read write))
 (import chicken)
 (require-extension extras)
 (import (only extras read-line))
 (import (only data-structures identity string-split))
 (use srfi-1 srfi-13 srfi-14 irregex)

 (include "ldif-core-impl.scm")
 )
