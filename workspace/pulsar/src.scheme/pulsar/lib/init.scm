(display-warn "Welcome to pulsar a drum machine!")
(newline-warn)

; Add the .kawapad configuration directory to import path.  The configuration
; directory should be on the user's home directory.

; (java.lang.System:set-property "kawa.import.path"
;                                (string-append (java.lang.System:get-property "kawa.import.path" "")
;                                               ":"
;                                               (java.lang.System:get-property "user.home" "")
;                                               "/.pulsar"))
; 

; (Wed, 07 Aug 2019 17:45:48 +0900)
(java.lang.System:set-property "kawa.import.path"
                               (string-append 
                                 (let ((p (java.lang.System:get-property "kawa.import.path" ) ))
                                   (if (eq? p #!null) "" (string-append p ":" )))
                                 (let ((p (java.lang.System:get-property "user.home")))
                                   (if (eq? p #!null) "" (string-append p "/" )))
                                 ".pulsar"))


; vim: expandtab :
