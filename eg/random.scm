(use gauche.threads)
(use rfc.http)

(use sxml.tools)

(use violet)
(use makiki)

;;
;; Application
;;

(define (create-page . children)
  `(html
    (@ (lang "en"))
    (head
     (meta (@ (charset "utf-8")))
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
     (meta (@ (name "description") (content "")))
     (meta (@ (name "author") (content "Mark Otto, Jacob Thornton, and Bootstrap contributors")))
     (title "Gauche Violet Demo")
     (link (@
            (rel "stylesheet")
            (integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T")
            (href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")
            (crossorigin "anonymous")))
     (style
         (string-append
          ".bd-placeholder-img {"
          "  font-size: 1.125rem;"
          "  text-anchor: middle;"
          "  -webkit-user-select: none;"
          "  -moz-user-select: none;"
          "  -ms-user-select: none;"
          "  user-select: none;"
          "}"
          "@media (min-width: 768px) {"
          "  .bd-placeholder-img-lg {"
          "    font-size: 3.5rem;"
          "  }"
          "}"
          ))
     (link (@ (rel "stylesheet") (href "/static/starter-template.css"))))
    (body
     (div (@ (id "fb-root")) "")
     (nav (@ (class "navbar navbar-expand-md navbar-dark bg-dark fixed-top"))
          (a (@ (href "#") (class "navbar-brand")) "Gauche Violet Demo")
          (button
           (@
            (type "button")
            (data-toggle "collapse")
            (data-target "#navbarsExampleDefault")
            (class "navbar-toggler")
            (aria-label "Toggle navigation")
            (aria-expanded "false")
            (aria-controls "navbarsExampleDefault"))
           (span (@ (class "navbar-toggler-icon"))))
          (div (@ (id "navbarsExampleDefault") (class "collapse navbar-collapse"))
               (ul (@ (class "navbar-nav mr-auto"))
                   (li (@ (class "nav-item active"))
                       (a (@ (href "/") (class "nav-link"))
                          "Home "))
                   (li (@ (class "nav-item"))
                       (a (@ (href "/static/") (class "nav-link"))
                          "Static")))))
     (main
      (@ (role "main") (class "container"))
      ,@children)
     (script (@ (src "/static/script.js")) "")
     (script (@
              (src "https://code.jquery.com/jquery-3.3.1.slim.min.js")
              (integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo")
              (crossorigin "anonymous"))
             "")
     (script (@
              (src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js")
              (integrity "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1")
              (crossorigin "anonymous"))
             "")
     (script (@
              (src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
              (integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM")
              (crossorigin "anonymous"))
             "")))
  )

(define (get-random)
  (call-with-input-file "/dev/random"
    (^p
     (let* ((ch (read-char p))
            (result (if (char? ch)
                        (let ((num (char->integer ch)))
                          (thread-sleep! (/ num 1000))
                          num)
                        (x->string ch))))
       result))))

(define-http-handler "/"
  (^[req app]
    (violet-async
     (^[await]
       (let* ((count (let ((n (await get-random)))
                       (if (integer? n) (+ 3(modulo n 5)) 1)))
              (nums (let loop ((count count) (dest ()))
                      (if (zero? count)
                          dest
                          (loop (- count 1)
                                (cons (await get-random) dest))))))
         (respond/ok req (cons "<!DOCTYPE html>"
                               (sxml:sxml->html
                                (create-page
                                 '(h1 "Here are some numbers from /dev/random:")
                                 `(table (@ (class "table"))
                                         ,@(map (^n `(tr (td ,(x->string n))))
                                                nums))
                                 )))))))))

(define-http-handler #/^\/static\// (file-handler))
