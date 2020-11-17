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

  (export init on-read on-new-connection on-write-done
		  dequeue-response! enqueue-task!
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

(define *client-vsock-table* (make-hash-table))

(define (close-stream vsock)
  (push-task! `(close ,(slot-ref vsock 'client))))

(define (on-write-done client)
  (let ((vsock (hash-table-get *client-vsock-table* client)))
	(dec-writes! vsock)
	(when (and (ref vsock 'closed?) (zero? (slot-ref vsock 'remaining-writes)))
	  (close-stream vsock))))

(define (make-output-port client)
  (define (respond-to-client str)
	(inc-writes! (hash-table-get *client-vsock-table* client))
    (push-task! `(res ,client ,str)))
  (define (close)
    ;; Do nothing here. The socket will be closed after all the tasks are done.
    )
  (make <virtual-output-port> :puts respond-to-client :close close))

(define (add-vsock! client iport)
  (let ((vsock (make <violet-socket>
				 :client client
				 :input-port iport
				 :output-port (make-output-port client))))
	(hash-table-put! *client-vsock-table* client vsock)
	vsock))

(define (on-read client buf)
  (let* ([iport (open-input-string buf)]
         [vsock (or (hash-table-get *client-vsock-table* client #f)
					(add-vsock! client iport))])
    (enqueue-task! (lambda ()
                     (with-module makiki (handle-client #f vsock))))
    ))

(define-class <violet-socket> (<connection>)
  ((client :init-value #f :init-keyword :client)
   (input-port :init-value #f :init-keyword :input-port)
   (output-port :init-value #f :init-keyword :output-port)
   (addr :init-value (car (make-sockaddrs "localhost" 2222)))
   (closed? :init-value #f)
   (remaining-writes :init-value 0)))


;; (define-generic connection-address-name)
;; (define-method connection-address-name ((a <string>)) a)

(define-method inc-writes! ((vsock <violet-socket>))
  (inc! (slot-ref vsock 'remaining-writes)))

(define-method dec-writes! ((vsock <violet-socket>))
  (dec! (slot-ref vsock 'remaining-writes)))

(define-method connection-input-port ((vsock <violet-socket>))
  (slot-ref vsock 'input-port))
(define-method connection-output-port ((vsock <violet-socket>))
  (slot-ref vsock 'output-port))

(define-method connection-peer-address ((vsock <violet-socket>))
  (slot-ref vsock 'addr))
(define-method connection-self-address ((vsock <violet-socket>))
  (slot-ref vsock 'addr))

(define-method connection-close ((vsock <violet-socket>))
  (when (ref vsock 'closed?)
        (set! (ref vsock 'closed?) #t)))

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
