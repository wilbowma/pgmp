#lang scribble/base 
@(require "defs.rkt")
Meta-programming is a technique for creating programs by specifying the
behavior in a meta-language that generates programs in a source
language. Like a compiler, when a meta-program generates code, it may
impose constraints not specified in the original program. 

Profile-guided optimization (PGO) is a compiler technique that uses
information such as execution counts, gathered from a sample run of a
program, to inform optimization decisions.  PGO done in the
meta-language would allow meta-programs to take advantage of the
original specification to generate more optimized code. This would
enable high-level optimizations not possible further along in a
compiler. Until now, meta-programmers have not had access to profile
information. 

This work presents a technique for doing profile-guided optimizations
in meta-programs. We present a profiling system that gives per-source
expression profile information, provides a way to access this profile
information in the meta-language and at runtime, and work with
traditional PGOs. This system is implemented in the high-performance
Chez Scheme compiler, and used in implementing an internal Cisco
project.

@;Meta-programming enables creating new, high-level constructs to express
@;the behavior of programs. Meta-programs are similar to compilers---they
@;enable reasoning in a higher-level language but produce code in a
@;lower-level language. To generate efficient source programs,
@;meta-programmers need the same tools and techniques as compiler writers.
@;
@;This profile information can
@;be more precise than static heuristics. For instance, profile
@;information can say exactly how many times a loop body is executed,
@;while static heuristics can only estimate.

