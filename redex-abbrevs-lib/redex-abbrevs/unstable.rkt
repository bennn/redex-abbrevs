#lang racket/base

(provide ;; Unstable provides

  *term-equal?*
  ;; Controls the default equality test for `check-mf-apply*`
  ;; Undocumented.

  step/deterministic
  ;; Apply a reduction relation to a term,
  ;;  fail-unless there is exactly one result

  reflexive-transitive-closure/deterministic
)

(require redex-abbrevs/private/redex-abbrevs)
