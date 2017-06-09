#lang racket

(provide
  *term-equal?*
  check-mf-apply*
  reflexive-transitive-closure/deterministic
  (rename-out
    [reflexive-transitive-closure/deterministic make--->*]))

(require
  (only-in rackunit with-check-info* check make-check-location)
  redex/reduction-semantics
  syntax/macro-testing
  (for-syntax racket/base racket/syntax syntax/parse syntax/srcloc))

;; =============================================================================

(define *term-equal?* (make-parameter equal?))

(define (reflexive-transitive-closure/deterministic --->)
  (define error-name (string->symbol (format "~a*" (object-name --->))))
  (lambda (t)
    (define v* (apply-reduction-relation* ---> t))
    (cond
     [(null? v*)
      (raise-user-error error-name "no result for ~a" t)]
     [(null? (cdr v*))
      (car v*)]
     [else
      (raise-user-error error-name "multiple results ~a --->* ~a" t v*)])))

(define-syntax (check-mf-apply* stx)
  (syntax-parse stx
   [(_ (~optional (~seq #:is-equal? ?eq:expr) #:defaults ([?eq #'#f])) [?e0 ?e1] ...)
    (quasisyntax/loc stx
      (let ([eq (or ?eq (*term-equal?*))])
        #,@(for/list ([kv (in-list (syntax-e #'((?e0 ?e1) ...)))])
             (define e0 (car (syntax-e kv)))
             (define e1 (cadr (syntax-e kv)))
             (quasisyntax/loc e0
               (with-check-info* (list (make-check-location '#,(build-source-location-list e0)))
                 (λ () (check eq (term #,e0) (term #,e1))))))))]))

