(use rfc.uri)
(use rfc.822)
(use util.queue)

(define (hello)
  (print "hello from script file!"))

(define *response-queue* (make-queue))

(define (dequeue-response!)
  (dequeue! *response-queue* #f)
  )

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
                   #;[host ($ rfc822-header-ref hdrs "host"
                            $ sockaddr-name $ socket-getsockname csock)]
                   #;[req (make-request line csock meth host path #f query hdrs)])
              (print path)
              (print hdrs)
              (let1 content #`"HTTP/1.1 200 OK\nContent-Type: text/html\n\nhello ,client\n"
                    (enqueue! *response-queue* (cons client content))
                    (enqueue! *response-queue* (cons client 'eof)))
              #;(unwind-protect
               (match (find-handler path req app)
                 [(handler req) (handler req app)]
                 [_ (respond/ng req 404)])
               ;; Clean temp files created by with-post-parameters
               ;; NB: We can use a parameter, assuming one thread handles
               ;; one request at a time.  If we introduce coroutines
               ;; (a thread may switch handling requests), we need to avoid
               ;; using cgi-temporary-files.
               (for-each sys-unlink (cgi-temporary-files)))))]
      #;[#/^[A-Z]+.*/ () (respond/ng (make-ng-request #`"[E] ,line" csock) 501)]
      #;[else (respond/ng (make-ng-request #`"[E] ,line" csock) 400)]))

  )
