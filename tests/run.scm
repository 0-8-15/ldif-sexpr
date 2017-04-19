(import (prefix ldif-parse2sexpr ldif:))

(define tr #<<EOF
### Sowas
    aber auch

dn: soso=gaga
nm: Gi
 ck
url:<gacks://gick
nm;a2: Gacker



dn: DN=nm2
nm: Gacks
uss:: QUIKQ0Q=
nm: Blubber

EOF
  )

(display (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read)))))

(use ports)

(define dollarref
  (let ((rx (irregex '(: bos "${" ($ (+ alphanumeric)) "}"))))
    (lambda (s i)
      (and-let* ((m (irregex-search rx s i)))
		(cons `(VARREF ,(irregex-match-substring m 1)) (irregex-match-end-index m))))))

(display
 (with-input-from-file
    "/home/u/b1/qemu/tmp/sysroots/qemuarm/usr/share/samba/setup/provision.ldif"
  (lambda()
    (port-fold cons '() (lambda () (ldif:read (current-input-port) value-converter: dollarref)))
    #;(list (ldif:read) (ldif:read) (ldif:read)))))

(display (ldif:rfc4514-read "1.1.200.4 = a+2=X,b=3 , b=4" 0))
(display (ldif:rfc4514-write (ldif:rfc4514-read "1.1.200.4 = a\\0a+2=X,b=3 , b=4" 0)))
(newline)
(for-each ldif:write (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read)))))

(exit 0)
