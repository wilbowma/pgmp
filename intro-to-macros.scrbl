#lang scribble/sigplan
@(require "defs.rkt")
@title[#:tag "intro-to-macros" "Meta-programming in Scheme"]
As we present our examples using Scheme and Scheme macros, we briefly
present Scheme macros and compare them with other meta-programming
facilities.

@racketblock[
(define-syntax fact
  (lambda (x)
    (syntax-case x ()
      [(_ n) 
       (let ([i (syntax->datum #'n)])
         (if (zero? i)
             #'1
             (with-syntax ([m (sub1 i)])
               #'(* n (fact m)))))])))

] defines the macro @racket[fact].

Macros are functions that run at compile time, taking syntax
as input and producing syntax as output. The body can be arbitrary
Scheme code. The macro is called like @racket[(fact 5)], but runs at
compile time. In this case, @racket[(fact 5)] produces the program
@racket[(* 5 (* 4 (* 3 (* 2 1))))], but the multiplication happens at
runtime.

This example could be written using C++ templates as follows
@todo{cite wikipedia?}
@verbatim{
template <int n>
struct fact {
  enum { value = n * fact<n - 1>::value };
};
           
template <>
struct fact<0> {
  enum { value = 1 };
}
}
