(use gauche.threads)
(use rfc.http)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use kaheka)

(add-load-path "./lib/")
(use violet)

;;
;; Application
;;

(define-http-handler "/"
  (^[req app]
    (thread-start!
     (make-thread
      (^[]
        (print "thread starting")
        (let-values (((status header body)
                      (http-get "numbersapi.com" "/random/math?json")))
          (respond/ok req `(sxml (html (body (h1 "It worked!") (pre ,body)))))))))))
