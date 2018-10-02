(use rfc.uri)
(use rfc.822)
(use data.queue)
(use gauche.vport)
(use gauche.net)

(add-load-path "../gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(define (init)
  (print "hello from script file!")
  #;(car #f))

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

(define (make-output-port client)
  (define (respond-to-client str)
    (push-task! `(res ,client ,str)))
  (define (close)
    (print "virtual-output-port closed")
    )
  (make <virtual-output-port> :puts respond-to-client :close close))

(define-http-handler "/"
  (^[req app] (respond/ok req "<h1>It worked!</h1>")))

(define (on-read client buf)
  (let* ([iport (open-input-string buf)]
         #;[line (read-line iport)]
         [vsock (make <violet-socket>
                  :client client
                  :input-port iport
                  :output-port (make-output-port client)
                  )])
    (with-module makiki (handle-client #f vsock))
    #;(rxmatch-case line
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

(define-class <violet-socket> ()
  ((client :init-value #f :init-keyword :client)
   (input-port :init-value #f :init-keyword :input-port)
   (output-port :init-value #f :init-keyword :output-port)
   (addr :init-value (car (make-sockaddrs "localhost" 2222))))
)

(define-method virtual-socket-input-port ((vsock <violet-socket>))
  (slot-ref vsock 'input-port))
(define-method virtual-socket-output-port ((vsock <violet-socket>))
  (slot-ref vsock 'output-port))

(define-method virtual-socket-getpeername ((vsock <violet-socket>))
  (slot-ref vsock 'addr))
(define-method virtual-socket-getsockname ((vsock <violet-socket>))
  (slot-ref vsock 'addr))

(define-method virtual-socket-close ((vsock <violet-socket>))
  (print "socket-close called")
  (push-task! `(close ,(slot-ref vsock 'client)))
  )

(define-method virtual-socket-shutdown ((vsock <violet-socket>) param)
  (print "socket-shutdown called")

  )

;; make-ng-request -> socket-getpeername
;; make-request -> socket-getpeername
;; %socket-discard -> socket-close socket-shutdown

(define *task-id* 0)
(define (push-task! task)
  (inc! *task-id*)
;;  (print #`"task: ,*task-id* ,task")
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
