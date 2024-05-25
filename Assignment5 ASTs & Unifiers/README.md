# Representing Abstract Syntax trees and finding the Most-General-Unifier

Consider the representation of trees ("pre-terms") using the following data type definition
type tree = V of string | C of { node: symbol ; children: tree list };;
with suitable type representations for types symbol and signature.

- A signature consists of symbols and their arities (>= 0) in the form of a list of (symbol, arity) pairs.
- The function check_sig that checks whether the signature is a valid signature (no repeated symbols, arities are non-negative etc.)
- The function wftree that checks that a given tree (pre-term) is well-formed according to the signature.
- The function mirror, which given a tree t returns a tree that is the mirror image of t. That is, at each level for each node, its children are reversed.
- Substitutions and Composition of Substitutions are included.
- The function mgu that given two well formed trees (terms) t1 and t2, returns their most general unifier, if it exists and otherwise raises an exception.
