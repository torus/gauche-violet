(define-module violet
  (use rfc.uri)
  (use rfc.822)
  (use rfc.http)
  (use data.queue)
  (use gauche.vport)
  (use gauche.net)
  (use gauche.threads)
  (use gauche.connection)

  (add-load-path "." :relative)
  (use rheingau)
  (rheingau-use makiki)

  (export init on-read on-new-connection dequeue-response! enqueue-task!
          violet-async)
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
          (guard (exc [else (report-error exc)])
                 (task))             ; task may return multiple times!
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
                     (with-module makiki (handle-client #f vsock))))
    ))

(define-class <violet-socket> (<connection>)
  ((client :init-value #f :init-keyword :client)
   (input-port :init-value #f :init-keyword :input-port)
   (output-port :init-value #f :init-keyword :output-port)
   (addr :init-value (car (make-sockaddrs "localhost" 2222)))
   (closed? :init-value #f))
)


;; (define-generic connection-address-name)
;; (define-method connection-address-name ((a <string>)) a)

(define-method connection-input-port ((vsock <violet-socket>))
  (slot-ref vsock 'input-port))
(define-method connection-output-port ((vsock <violet-socket>))
  (slot-ref vsock 'output-port))

(define-method connection-peer-address ((vsock <violet-socket>))
  (slot-ref vsock 'addr))
(define-method connection-self-address ((vsock <violet-socket>))
  (slot-ref vsock 'addr))

(define-method connection-close ((vsock <violet-socket>))
  (if (ref vsock 'closed?)
      (print #`"double closing attempted: ,vsock")
      (begin
        (set! (ref vsock 'closed?) #t)
        (push-task! `(close ,(slot-ref vsock 'client)))))
  )

(define-method connection-shutdown ((vsock <violet-socket>) param)
  ;; Not sure what should be done here??
  )

(define *task-id* 0)
(define (push-task! task)
  (inc! *task-id*)
  (enqueue! *response-queue* (cons *task-id* task)))

;;;;;

(define (await yield)
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
  (enqueue-task!
   (^[]
     (call/cc (lambda (yield)
                (func (await yield)))))))
