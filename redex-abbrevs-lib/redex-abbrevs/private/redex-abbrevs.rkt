#lang racket/base

(provide
  *term-equal?*
  define-language++
  check-mf-apply*
  check-judgment-holds*
  check-not-judgment-holds*
  reflexive-transitive-closure/deterministic
  step/deterministic
  (rename-out
    [reflexive-transitive-closure/deterministic make--->*]))

(require
  (only-in rackunit
    with-check-info* check check-true check-false make-check-location make-check-info)
  redex/reduction-semantics
  syntax/macro-testing
  (for-syntax racket/base racket/syntax syntax/parse syntax/srcloc))

;; =============================================================================

(define *term-equal?* (make-parameter equal?))

(define (step/deterministic ---> t)
  (define t* (apply-reduction-relation ---> t))
  (cond
   [(null? t*)
    (raise-user-error 'step/deterministic "reduction relation ~a is stuck for term ~a" (object-name --->) t)]
   [(null? (cdr t*))
    (car t*)]
   [else
    (raise-user-error 'step/deterministic "reduction relation ~a is non-deterministic for term ~a,~n  next states: ~a" (object-name --->) t t*)]))

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

(define-for-syntax (make-check-judgment-holds* check-stx stx)
  (syntax-parse stx
   [(_ t*:expr ...)
    (quasisyntax/loc stx
      (begin .
        #,(for/list ([t (in-list (syntax-e (syntax/loc stx (t* ...))))])
            (quasisyntax/loc t
              (with-check-info* (list (make-check-location '#,(build-source-location-list t)))
                (λ () (#,check-stx (judgment-holds #,t))))))))]))

(define-syntax (check-judgment-holds* stx)
  (make-check-judgment-holds* #'check-true stx))

(define-syntax (check-not-judgment-holds* stx)
  (make-check-judgment-holds* #'check-false stx))

(define-syntax (check-mf-apply* stx)
  (syntax-parse stx
   [(_ (~optional (~seq #:is-equal? ?eq:expr) #:defaults ([?eq #'#f])) [?e0 ?e1] ...)
    (quasisyntax/loc stx
      (let ([eq (or ?eq (*term-equal?*))])
        (void)
        #,@(for/list ([stx (in-list (syntax-e #'((?e0 ?e1) ...)))])
             (define kv (syntax-e stx))
             (define k (car kv))
             (define loc (build-source-location-list k))
             (define e0
               (syntax-parse k
                [(mf . arg*)
                 (syntax/loc k (mf-apply mf . arg*))]
                [_
                 (raise-syntax-error 'check-mf-apply* "expected a metafunction application" stx #'x '())]))
             (define e1 (cadr kv))
             (quasisyntax/loc e0
               (with-check-info* (list (make-check-location '#,loc))
                 (λ () (check eq (term #,e0) (term #,e1))))))))]))

(begin-for-syntax
  (define-syntax-class non-terminal
    #:attributes (nt*)
    (pattern (nt:id ... (~literal ::=) pat* ...)
     #:attr nt* #'(nt ...))
    (pattern (nt:id pat* ...)
     #:attr nt* #'(nt))
    (pattern ((nt:id ...) pat* ...)
     #:attr nt* #'(nt ...))))

(define-syntax (define-language++ stx)
  (syntax-parse stx
   [(_ lang-name:id
       (~optional (~seq #:alpha-equivalent? alpha=?:id) #:defaults ((alpha=? #'#f)))
       nt-def*:non-terminal ...
       . other)
    #:with define-alpha=?
           (if (syntax-e #'alpha=?)
             (syntax/loc stx (define (alpha=? t0 t1) (alpha-equivalent? lang-name t0 t1)))
             (syntax/loc stx (void)))
    #:with (define-predicate* ...)
           (for*/list ([nt* (in-list (syntax-e #'(nt-def*.nt* ...)))]
                       [nt (in-list (syntax-e nt*))])
             (define nt? (format-id stx "~a?" (syntax-e nt)))
             (quasisyntax/loc stx
               (define #,nt? (redex-match? lang-name #,nt))))
    (quasisyntax/loc stx
      (begin
        (define-language lang-name nt-def* ... . other)
        define-alpha=?
        define-predicate* ...))]))
