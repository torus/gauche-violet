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

(define (violet-add-task! proc)
  (enqueue-task! proc))

(define (violet-await yield)
  (^[proc]
    (call/cc (lambda (cont)
               (thread-start! 
                (make-thread
                 (^[]
                   (let ((result 
                          (guard (exc [else (x->string exc)])
                                 (proc))))
                     (enqueue-task! (^[] (cont result)))))))
               (yield)))))

(define (violet-async func)
  (violet-add-task!
   (^[]
     (call/cc (lambda (yield)
                (func (violet-await yield)))))))

(define (get-random)
  (call-with-input-file "/dev/random"
    (^p
     (let* ((ch (read-char p))
            (result (if (char? ch)
                        (let ((num (char->integer ch)))
                          (thread-sleep! (/ num 1000))
                          (number->string num))
                        (x->string ch))))
       result))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let ((content (await get-random))
             (content2 (await get-random)))
         (respond/ok req `(sxml (html (body (h1 "It worked!!!")
                                            (pre ,content) (pre ,content2))))))))))
