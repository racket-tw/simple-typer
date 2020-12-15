#lang racket

(module+ main
  (require racket/cmdline)

  (define who (box "world"))
  (command-line
    #:program "racket-project"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))

(module+ test
  (require rackunit)

  (test-case
    "Example Test")
    (check-equal? 1 1)

  (test-equal? "Shortcut Equal Test" 1 1))
