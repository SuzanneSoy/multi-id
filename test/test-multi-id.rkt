#lang typed/racket

(require multi-id
         type-expander
         typed/rackunit
         (for-syntax racket/list))

;; Inject in this file the tests shown in multi-id.hl.rkt
(begin
  (require (for-syntax (submod "../multi-id.hl.rkt" test-syntax)
                       syntax/strip-context))
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))

  (insert-tests))
