(use rfc.uri)
(use rfc.822)
(use data.queue)

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
                   )
              (print path)
              (print hdrs)
              (respond-hello client path hdrs)
              ))]
      ))
  )

(define (respond-hello client path headers)
  (print "respond-hello running")
  (let1 content #`"HTTP/1.1 200 OK\nContent-Type: text/html\n\nhello ,client ,path\n"
        (enqueue! *response-queue* (cons client content))
        (enqueue! *response-queue* (cons client 'eof))))
