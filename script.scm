(use rfc.uri)
(use rfc.822)
(use data.queue)
(use gauche.vport)
(use gauche.net)
(use gauche.threads)

(add-load-path "../gauche-rheingau/lib/")
(use rheingau)
(rheingau-use makiki)

(define *task-queue* (make-mtqueue))

(define (init)
  (print "Starting worker thread.")
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (let ((task (dequeue/wait! *task-queue*)))
          (print "got task")
          (task))
        (loop))))))

(define *response-queue* (make-queue))

(define (dequeue-response!)
  (dequeue! *response-queue* #f)
  )

(define (on-new-connection)
  (print "new connection!"))

(define (make-output-port client)
  (define (respond-to-client str)
    (push-task! `(res ,client ,str)))
  (define (close)
    (print "virtual-output-port closed"))
  (make <virtual-output-port> :puts respond-to-client :close close))

(define-http-handler "/"
  (^[req app] (respond/ok req "<h1>It worked!</h1>")))

(define (on-read client buf)
  (let* ([iport (open-input-string buf)]
         [vsock (make <violet-socket>
                  :client client
                  :input-port iport
                  :output-port (make-output-port client)
                  )])
    (print "enqueuing")
    (enqueue! *task-queue* (lambda ()
                             (with-module makiki (handle-client #f vsock))))
    ))

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
