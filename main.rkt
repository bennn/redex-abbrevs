#lang racket/base

(provide ;; Documented provides

  define-language++
  ;; Just like `define-language`, but:
  ;; - creates predicates for each non-terminal
  ;; - accepts optional argument to define alpha-equivalence function

  check-mf-apply*
  ;; (check-mf-apply* #:is-equal? eq (actual expect) ...)
  ;; Same as writing `(begin (check eq (term actual) (term expect)) ...)`
  ;;  except requires fewer keystrokes.

  check-judgment-holds*
  check-not-judgment-holds*
  ;; (check-judgment-holds* t ...)
  ;; Same as writing `(begin (check-true (judgment-holds t)) ...)`
  ;;  or `(begin (check-false (judgment-holds t)) ...)`
  ;;  respectively.

  make--->*
  ;; (-> reduction-relation? (-> term? term?))
  ;; Compute the determinsitic reflexive transitive closure of a reduction relation.
)

(require redex-abbrevs/private/redex-abbrevs)
