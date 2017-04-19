;; TBD: Make a better test suite.

(use (prefix ldif-sexpr ldif:))

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

(assert (= (length (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read))))) 2))

(assert (equal? (ldif:ldif-attributes (with-input-from-string "dn: CN=foobar\nfoo: bar" ldif:read))
	 '(("foo" "bar"))))

#|
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
|#

(assert (equal? (ldif:rfc4514-read "1.1.200.4 = a+2=X,b=3 , b=4" 0) '((("1.1.200.4" "a") ("2" "X")) (("b" "3")) (("b" "4")))))

(assert (equal? (with-output-to-string
		  (lambda ()
		    (ldif:rfc4514-write (ldif:rfc4514-read "1.1.200.4 = a\\0a+2=X,b=3 , b=4" 0))))
		"1.1.200.4=a\\0A+2=X,b=3,b=4"))

(assert (equal? (with-output-to-string
		  (lambda ()
		    (for-each ldif:write (with-input-from-string tr (lambda () (list (ldif:read) (ldif:read)))))))
		#<<EOF
dn: soso=gaga
nm;a2: Gacker
url:<gacks://gick
nm: Gick

dn: DN=nm2
nm: Blubber
uss:: QUIKQ0Q=
nm: Gacks


EOF
))

(assert (equal? (ldif:ldap-filter-string
		 '(and (exists name) (= foo "bar")))
		"(&(name=*)(foo=bar))"))

(exit 0)
