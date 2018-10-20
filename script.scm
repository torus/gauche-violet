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

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[yield]
       (let ((content (yield
                       (^[]
                         (call-with-input-file "/dev/random"
                           (^p
                            (let* ((ch (read-char p))
                                  (result (if (char? ch)
                                              (number->string (char->integer ch))
                                              (x->string ch))))
                              result)))))))
         (respond/ok req `(sxml (html (body (h1 "It worked!!!") (pre ,content))))))))))

