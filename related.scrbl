#lang scribble/base
@(require "defs.rkt")
@(require "bib.rkt")
@(require scribble/manual)
@(require scriblib/footnote)
@(require scriblib/figure)
@title[#:tag "related" "Related and Future Work"]
@section{Low-level PGO}
Modern systems such as GCC, .NET, and LLVM use profile directed
optimizations@~cite[lattner02 gcc .net]. These systems uses profile
information to guide decisions about code positioning, register
allocation, inlining, and branch optimizations.

GCC profiles an internal control-flow graph (CFG). To maintain a
consistent CFGs across instrumented and optimization builds, GCC
requires similar optimization decisions across builds@~cite[chen10]. In
addition to the common optimizations noted previously, .NET extends
their profiling system to probe values in @racketkeywordfont{switch}
statements. They can use this value information to reorder optimize
@racketkeywordfont{switch} branches, similar to the implementation of
@racket[case] we presented in @secref{eg-case}. 

Our system supports all these optimizations and has several advantages.
While .NET extends their profiling system to get additional
optimizations, we can support all the above optimizations in a single
general-purpose system. By using profile information associated with
source expressions, we reduce reliance specific internal compiler
decisions and make profile information more reusable. When there is
no substitute for block-level information, such as when reordering basic
blocks, we support both source and block profiling in the same system.

@section{Dynamic Recompilation}
The standard model for PGO requires the instrument-profile-optimize
workflow. LLVM has a different model for PGO. LLVM uses a runtime
reoptimizer that monitors the running program. The runtime can profile
the program as it runs ``in the field'' and perform simple optimizations
to the machine code, or call to an offline optimizer for more complex
optimiztions on the LLVM bytecode. 

While not currently enabled, our mechanism supports this kind of
reoptimization. We build on the work of Burger and Dybvig, who present
an infrastructure for profile-directed dynamic
reoptimization@~cite[burger98]. Their work shows just 14% run-time
overhead for instrumented code, but they express concerns that dynamic
recompilation will not overcome this cost. Our internal benchmarks show
similar overhead. To enable dynamic PGO, we would need to modify our
mechanism to automatically reload profile information, such as whenever
@racket[profile-query-weight] is called, instead of manually loading
information from a file. This is a trivial change to our system, but we
have not optimizations in mind that make use of profile-guided at
runtime.

@section{Meta-program optimizations}
Meta-programming has proven successful at providing high-levels of
abstraction while still producing efficient code. Meta-programming has
been used to implement abstract libraries@~cite[boost]@todo{STL?}, 
domain specific languages@~citea["sujeeth13" "flatt09"], and even whole
general purpose languages@~citea["rafkind12" "tobin-hochstadt11"
"tobin-hochstadt08" "barzilay05"]. These meta-programs can lose or
obscure information during the translation into target-language code. 

We're not the first to realize this. Many meta-program optimizations
exist. Tobin-Hochstadt et. al. implement the optimizer for Typed Racket
as a meta-program@~citea{tobin-hochstadt11}. Sujeeth et. al. provide a
framework for generated optimized code from DSLs @~citea{sujeeth13}.
Hawkins et. al. implement a compiler for a language that generates C++
implementations of data structures based on high-level
specifications@~citea["hawkins11" "hawkins12"]. 

Even using profile information to perform optimizations in meta-programs
is not new. Chen et. al. implement their own profile and meta-program
tools to provide a profile-guided meta-program for performing process
placement for SMP clusters@~citea{chen06}. Liu and Rus provide a tools
that uses profile information to identify suboptimial usage of the C++
STL. 

We support these works by providing a single, general-purpose mechanism
in which we can implement new languages, DSLs, abstract libraries, and
arbitrary meta-programs, all taking advantage of profile-guided
optimizations.

@section{More PGO}
We have previously presented some past work on both low-level PGOs and
profile-guided meta-programs. But the use of profile information
is still an active area of research. Furr et. al. present a system for
inferring types in dynamic languages to assist in
debugging@~citea{furr09}. Chen et. al. use profile information to
reorganize the heap and optimize garbage collection@~citea{chen06}. Luk
et. al. use profile information to guide data
prefetching@~citea{luk02}. Debray and Evans use profile information to
compress infrequently executed code on memory constrained
systems@~citea{debray02}. 

With so many profile-guided optimizations, we need a general-purpose
mechanism in which to implement them without reimplementing profiling,
compiling, and meta-programing tools.

@; PDO has some limitations. If profile data is inaccurate it can slow down
@; a program by optimizing for the wrong cases. This can happen when
@; programs are profiled on a set of benchmarks that don't represent the
@; real-world parameters of a program. The need to compile twice and run
@; the program leads to a longer development process. These limitations
@; make developers unlikely to even use PDOs @todo{cite 2, 7}. we
@; find 

@;http://dl.acm.org/citation.cfm?id=582432


