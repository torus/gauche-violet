(define-module violet
  (use rfc.uri)
  (use rfc.822)
  (use rfc.http)
  (use data.queue)
  (use gauche.vport)
  (use gauche.net)
  (use gauche.threads)

  (add-load-path "./gauche-rheingau/lib/")
  (use rheingau)
  (rheingau-use kaheka)

  (export init on-read on-new-connection dequeue-response! enqueue-task!)
)

(select-module violet)

(define *task-queue* (make-mtqueue))

(define (enqueue-task! proc)
  (enqueue! *task-queue* proc))

(define (init)
  (print "Starting worker thread.")
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (let ((task (dequeue/wait! *task-queue*)))
          (task)
          (flush))
        (loop))))))

(define *response-queue* (make-queue))

(define (dequeue-response!)
  (dequeue! *response-queue* #f)
  )

(define (on-new-connection)
  )

(define (make-output-port client)
  (define (respond-to-client str)
    (push-task! `(res ,client ,str)))
  (define (close)
    ;; Do nothing here. The socket will be closed after all the tasks are done.
    )
  (make <virtual-output-port> :puts respond-to-client :close close))

(define (on-read client buf)
  (let* ([iport (open-input-string buf)]
         [vsock (make <violet-socket>
                  :client client
                  :input-port iport
                  :output-port (make-output-port client)
                  )])
    (enqueue-task! (lambda ()
                     (with-module kaheka (handle-client #f vsock))))
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
  (push-task! `(close ,(slot-ref vsock 'client)))
  )

(define-method virtual-socket-shutdown ((vsock <violet-socket>) param)
  ;; Not sure what should be done here??
  )

(define *task-id* 0)
(define (push-task! task)
  (inc! *task-id*)
  (enqueue! *response-queue* (cons *task-id* task)))
