(use gauche.threads)
(use rfc.http)

(add-load-path "./gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(add-load-path "./lib/")
(use violet)

;;
;; Application
;;

(define (get-random)
  (call-with-input-file "/dev/random"
    (^p
     (let* ((ch (read-char p))
            (result (if (char? ch)
                        (let ((num (char->integer ch)))
                          (thread-sleep! (/ num 1000))
                          num)
                        (x->string ch))))
       result))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((count (let ((n (await get-random))) (if (integer? n) (modulo n 10) 1)))
              (nums (let loop ((count count) (dest ()))
                      (if (zero? count)
                          dest
                          (loop (- count 1)
                                (cons (await get-random) dest))))))
         (respond/ok req `(sxml (html (body (h1 "Random Numbers")
                                            ,@(map (^n `(pre ,(x->string n))) nums)
                                            )))))))))
