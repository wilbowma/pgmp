#lang scribble/base 
@(require "defs.rkt")
Meta-programming is a technique for creating programs by defining
meta-language constructs that generate programs in a source language.
Meta-programming enables creating new, high-level constructs to express
the behavior of programs. Meta-programs are similar to compilers---they
enable reasoning in a higher-level language but produce code in a
lower-level language. To generate efficient source programs,
meta-programmers need the same tools and techniques as compiler writers.

Profile-directed optimization (PDO) is a compiler technique that uses
information such as execution counts, gathered from a sample run of a
program, to inform optimization decisions. This profile information can
be more precise than static heuristics. For instance, profile
information can say exactly how many times a loop body is executed,
while static heuristics can only estimate.

Until now, meta-programmers have not had access to profile information.
This work presents a technique for doing profile-directed optimizations
in meta-programs. We present a profiling system that gives per-source
expression profile information and provides a way to access this profile
information in the meta-language. This system is implemented in the
high-performance Chez Scheme compiler, and used in implementing an
internal Cisco project.
