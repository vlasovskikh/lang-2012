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

The next step is to create a O(N) implementation. We will do it by converting
a regexp AST into a non-determenistic automaton (NFA) and then running it on
an input string.

In order to create an NFA graph we need a graph data type. It is defined in
our `graph` module as an abstract data type. It means that we define its
interface (several constructor functions, as well as accessor and mutator
functions) and access graphs only via this interface without any knowledge of
its implementation details.

Here are some abstraction barriers that arise in our regexp implementation:

    +--------+
    |regexp()|
    +--------+
    | graph()|
    +--------+
    | dict() |
    +--------+

For example, users of the `regexp()` abstract data type don't have to know
anything about the NFA based on the `graph()` data type.


### Parsing

Regular expressions can parse only regular languages. But almost all real
computer languages are at least context-free (sometimes context-sensitive). Even
a simple nested parentheses language is not a regular language. So regular
expressions are not expressive enough for this kind of tasks and we should move
on to parsers of context-free languages.

One of the simplest ways to create a parser is to write a recursive parser
with backtracking. You can find an example of a recursive parser for the nested
parentheses language in the `nested_rec` module.

Writing a recursive parser manually is a tedious task. The resulting parser is
usually far from being declarative. It only slightly resembles the structure of
the language grammar. One of the most declarative and compact forms of
describing a language is the Backus-Naur Form, or BNF. But from an Erlang
programmer's point of view a BNF grammar of the language is just a string that
should be parsed and processed in order to become a real parsing code.

A simpler approach to declarative parsers is known as functional parsing
combinators. A parsing combinator is a function that is either a primitive
parser, or a composition of several parsers. The structure of a parser based on
parsing combinators is quite similar to the BNF of the language. The `funparse`
module defines a simple parsing combinators library. In the `nested` module you
may find a nested parentheses parser based on parsing combinators.

The first step in creating a parser is to write down a BNF grammar of your
language. We will use the Extended BNF (EBNF) in our grammar definitions.

A simple regexp EBNF:

    special-char  = ? some of \, (, ), *, etc. ?;
    escaped-char  = "\" , special-char;
    regular-char  = ? any char except special-char ?;
    char          = regular-char | escaped-char;
    group         = "(" , regexp , ")";
    char-class    = "[" , { char "-" char | char } , "]";
    term          = ( group | char | char-class ) , [ "*" | "?" ];
    seq           = { term };
    alt           = seq , [ "|" , alt ];
    regexp        = alt;

TODO


  [1]: http://erldocs.com/R15B/stdlib/re.html

