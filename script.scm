(use rfc.uri)
(use rfc.822)
(use data.queue)

(define (init)
  (print "hello from script file!"))

(define *response-queue* (make-queue))
(define *request-map* (make-tree-map))

(define (dequeue-response!)
  (dequeue! *response-queue* #f)
  )

(define (on-result id . content)
  (print #`"on-result ,id")
  (let ((proc (tree-map-get *request-map* id #f)))
    (when proc
          (tree-map-delete! *request-map* id)
          (apply proc content))))

(define (on-new-connection)
  (print "new connection!"))

(define (on-read client buf)
  (print client)

  (let* ([iport (open-input-string buf)]
         [line (read-line iport)])
    (rxmatch-case line
      [test eof-object?
            (respond/ng (make-ng-request "(empty request)" csock) 400
                        :no-response #t)]
      [#/^(GET|HEAD|POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
          (receive (auth path query frag) (uri-decompose-hierarchical abs-path)
            (let* ([path (uri-decode-string path :cgi-decode #t)]
                   [hdrs (rfc822-read-headers iport)]
                   )
              (print path)
              (print hdrs)
              (respond-hello client path hdrs)
              ))]
      ))
  )

(define *task-id* 0)
(define (push-task! task)
  (inc! *task-id*)
  (enqueue! *response-queue* (cons *task-id* task)))

(define (push-task/ret! task proc)
  (push-task! task)
  (tree-map-put! *request-map* *task-id* proc))

(define (respond-hello client path headers)
  (print "respond-hello running")

  (push-task/ret! '(get "http://numbersapi.com/random/math?json")
                  (lambda (result)
                    ;; (print result)
                    (push-task! `(res ,client
                                      "HTTP/1.1 200 OK\nContent-Type: application/json\n\n"))
                    (push-task! `(res ,client ,result))
                    (push-task! `(close ,client))
                    )))
