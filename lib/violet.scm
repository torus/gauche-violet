(define-module violet
  (use rfc.uri)
  (use rfc.822)
  (use rfc.http)
  (use data.queue)
  (use gauche.vport)
  (use gauche.net)
  (use gauche.threads)
  (use gauche.connection)
  (use control.plumbing)

  (use makiki)

  (export violet-init
          violet-on-read
          violet-on-new-connection
          violet-on-write-done
          violet-dequeue-response!
          violet-async)
)

(select-module violet)

(define *task-queue* (make-mtqueue))

(define (enqueue-task! proc)
  (enqueue! *task-queue* proc))

(define (violet-init)
  (print #"Initializing Violet ~|*violet-version*|...")
  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
         (let ((task (dequeue/wait! *task-queue*)))
           (guard (exc [else (report-error exc)])
                  (task))             ; task may return multiple times!
           (flush))
         (loop))))))

(define *response-queue* (make-mtqueue))

(define (violet-dequeue-response!)
  (dequeue! *response-queue* #f)
  )

(define (violet-on-new-connection)
  )

(define *client-vsock-table* (make-hash-table))

(define (close-stream vsock)
  (push-task! `(close ,(slot-ref vsock 'client))))

(define (violet-on-write-done client)
  (let ((vsock (car (hash-table-get *client-vsock-table* client))))
    (enqueue-task!
     (^[] (dec-writes! vsock)
       (when (and (ref vsock 'closed?)
                  (zero? (slot-ref vsock 'remaining-writes)))
         (hash-table-delete! *client-vsock-table* client)
         (close-stream vsock))))))

(define (make-output-port client)
  (define (respond-to-client str)
    (let ((vsock-and-proc (hash-table-get *client-vsock-table* client #f)))
      ;; do nothing if the client was already disconnected.
      (when vsock-and-proc
        (inc-writes! (car vsock-and-proc))
        (push-task! `(res ,client ,str)))))

  (define (close)
    ;; Do nothing here. The socket will be closed after all the tasks are done.
    )
  (make <virtual-output-port> :puts respond-to-client :close close))

(define (add-vsock! client buf)
  (let-values (((inlets outlets) (make-pipe)))
    (let* ((vsock (make <violet-socket>
                    :client client
                    :input-port (car outlets)
                    :output-port (make-output-port client)))
           (input-proc (^[buf]
                         (guard (exc [else (report-error exc)])
                                (display buf (car inlets))
                                (flush (car inlets))))))
      (hash-table-put! *client-vsock-table*
                       client (list vsock input-proc))
      (input-proc buf)

      (thread-start!
       (make-thread
        (^[]
          (guard (exc [else (report-error exc)])
                 (with-module makiki (handle-client #f vsock)))))))))

(define (violet-on-read client buf)
  (let ([bufcopy (string-copy buf)])
    (enqueue-task!
     (^[]
  (let ([vsock-and-proc (hash-table-get *client-vsock-table* client #f)])
    (if vsock-and-proc
        ((cadr vsock-and-proc) bufcopy)
        (add-vsock! client bufcopy)))))))

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
  (if (ref vsock 'closed?)
      (print #"double closing attempt: ~vsock")
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
                          (guard (exc [else (cons exc #f)])
                                 (cons #f (proc)))))
                     (enqueue-task! (^[] (if (car result)
                                             (raise (car result))
                                             (cont (cdr result)))))))))
               (yield)))))

(define (violet-async func)
  (enqueue-task!
   (^[]
     (call/cc (lambda (yield)
                (func (await yield)))))))
