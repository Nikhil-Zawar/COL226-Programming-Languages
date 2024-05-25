# Assignment Specification

A simple parser for the “object language” being a simple logic programming language (similar e.g., to a subset of Prolog).
The parser is written in Ocaml version of Yacc. The grammar defined is very similar to that of prolog with similar constructs for the surface language.

The abstract syntax of the logic programming language is represented as a data type in OCaml. 
Abstract syntax trees (terms in this data type) will be the output of the parser.

The abstract structure of a Prolog program is the following:
- A program is a sequence of clauses.
- A clause can either be a fact or a rule.
- A fact has a head but no body. A rule has a head and a body.
- The head is a single atomic formula.
- A body is a sequence of atomic formulas.
- An atomic formula is a k-ary predicate symbol followed by k terms.
- A term is either a variable, a constant, or a k-ary function symbol with k subterms.
- A goal is a sequence of atomic formulas.
