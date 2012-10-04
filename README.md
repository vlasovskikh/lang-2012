FP Languages in SPbSPU
======================


Regexp2
-------

The idea is to implement our own regular expression package similar to the
standard [re][1] module, but much simpler. Regular expressions can parse regular
languages. We will need regular expressions _later in our course_.

Our regular expression matcher must match a string in O(N) where N is the
length of the string.

We split the problem of creating a regexp matcher into two smaller problems:

1. Parsing (transform a regexp string into an AST)
2. Matching (using a finite state automaton in order to achieve O(N))


### Matching

First we will try to create a naive implementation of matching using a recursive
function. As we will see, it is not that easy to implement a correct
backtracking algorithm for `*` (for regexps like `a*a` and `a*ab`). And even a
correct backtracking implementation works in exponential time O(2^N).

TODO


### Parsing

"Normal" regexp EBNF:

    char      = ? any character ?;
    group     = "(" , regexp , ")"
    term      = ( group | char ) , [ "*" | "?" ];
    seq       = { term };
    alt       = seq , [ "|" , alt ];
    regexp    = alt;

TODO


  [1]: http://erldocs.com/R15B/stdlib/re.html

