(define-module scenario
  )


(select-module scenario)

(define (cmd-character . args)
  (let loop ((args args)
             (obj (make-character)))
    (if (null? args)
        obj
        (let ((head (car args))
              (tail (cdr args)))
          (let ((result (head obj)))
            (loop tail result))))))

(define (cmd-name name)
  (lambda (obj)
    (character-name-set! obj name)))

(define (cmd-label name)
  (lambda (obj)
    (character-label-set! obj name)))

