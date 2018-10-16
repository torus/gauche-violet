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
    (enqueue-task!
     (^[]
       (thread-start!
        (make-thread
         (^[]
           (let-values (((status header body)
                         (http-get "numbersapi.com" "/random/math?json")))
             (enqueue-task!
              (^[]
                (respond/ok req `(sxml (html (body (h1 "It worked!") (pre ,body)))))))))))))))

(define (violet-add-task! proc)
  (enqueue-task! proc))

(define (violet-await exit proc)
  (call/cc (lambda (cont)
             (thread-start! 
              (make-thread
               (^[]
                 (let ((result #?=(proc)))
                   #?=(enqueue-task! (^[] #?=(cont result)))))))
             (exit))))

(define-http-handler "/2"
  (^[req app]
    (violet-add-task!
     (^[]
       #?=(call/cc (lambda (outer)
                  (let ((content (violet-await
                               outer
                               (^[]
                                 #?=(let-values (((status header body)
                                               (http-get "numbersapi.com" "/random/math?json")))
                                      body)))))
                    #?=(respond/ok req `(sxml (html (body (h1 "It worked!!!") (pre ,content))))))))))))
