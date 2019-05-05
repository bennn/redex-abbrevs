#lang scribble/manual
@require[
  scribble/example
  (for-label racket/base redex/reduction-semantics racket/contract rackunit)]
@(define (make-ra-eval) (make-base-eval '(begin (require redex/reduction-semantics rackunit redex-abbrevs))))

@title{Redex Abbrevs}

@defmodule[redex-abbrevs]{
  Macros for working with @hyperlink["http://docs.racket-lang.org/redex/The_Redex_Reference.html"]{PLT Redex}.
}

@defform[(check-mf-apply* #:is-equal? eq (actual expected) ...)]{
  Expands to @racket[(begin (check eq (term actual) (term expected)) ...)],
   where @racket[eq] is @racket[equal?] by default.

  @examples[#:eval (make-ra-eval)
    (define-language N
      (nat ::= Z (S nat)))

    (define-metafunction N
      add2 : nat -> nat
      [(add2 nat)
       (S (S nat))])

    (check-mf-apply*
     ((add2 Z)
      (S (S Z)))
     ((add2 (S Z))
      (S (S (S Z)))))
  ]
}

@defform[(check-judgment-holds* t ...)]{
  Expands to @racket[(begin (check-true (judgment-holds t)) ...)]
}

@defform[(check-not-judgment-holds* t ...)]{
  Expands to @racket[(begin (check-false (judgment-holds t)) ...)]

  @examples[#:eval (make-ra-eval)
    (define-language N
      (nat ::= Z (S nat)))

    (define-judgment-form N
      #:mode (is-zero I)
      [
       ---
       (is-zero Z)])

    (check-not-judgment-holds*
      (is-zero (S Z)))
  ]
}

@defform[(define-language++ lang-name #:alpha-equivalent? alpha-name non-terminal-def ... maybe-binding-spec)]{
  Similar to @racket[define-language], but:
  @itemlist[
  @item{
    Defines a predicate for each @racket[_non-terminal-def].
    The predicates have type @racket[(-> any/c boolean?)] and return @racket[#true]
     for @tech[#:doc '(lib "redex/redex.scrbl")]{terms} that match the non-terminal.
  }
  @item{
    Accepts an optional keyword argument @racket[alpha-name].
    If given, binds @racket[alpha-name] to a function with type @racket[(-> any/c any/c boolean?)]
     such that @racket[(alpha-name term0 term1)] if and only if @racket[(alpha-equivalent? lang-name term0 term1)].
  }
  ]

  @examples[#:eval (make-ra-eval)
    (define-language++ N
      (nat ::= Z (S nat)))

    (nat? (term Z))
  ]
}

@defproc[(make--->* [--> reduction-relation?]) (-> any/c any/c)]{
  The result of @racket[(make--->* -->)] is a function on terms that acts like
   the reflexive-transitive closure of the reduction relation @racket[-->].
  If @racket[(apply-reduction-relation* --> t)] returns a list with one term,
   then @racket[((make--->* -->) t)] will return the same term.
  Otherwise, @racket[((make--->* -->) t)] will raise an exception.
}

@;@defmodule[redex-abbrevs/unstable]{
@;  Experimental features.
@;  May change or disappear at any time.
@;}
