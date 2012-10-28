-module(regexp2).
-author("vlan").
-export([run/2]).
-include_lib("eunit/include/eunit.hrl").


-type regexp() :: empty
                | {char, integer()}
                | {class, class_spec()}
                | {seq, [regexp()]}
                | {star, regexp()}.

-type class_spec() :: [class_component()].

-type class_component() :: {char, integer()}
                         | {range, integer(), integer()}.


run(S, Regexp) ->
  case match(S, Regexp, 0) of
    {match, _, Pos} -> {match, [{0, Pos}]};
    nomatch -> nomatch
  end.


match(S, empty, Pos) ->
  {match, S, Pos};
match([C|Cs], {char, C}, Pos) ->
  {match, Cs, Pos + 1};
match(_, {char, _}, _) ->
  nomatch;
match(S, {seq, []}, Pos) ->
  {match, S, Pos};
match(S, {seq, [Regexp|Regexps]}, Pos) ->
  case match(S, Regexp, Pos) of
    {match, S2, Pos2} -> match(S2, {seq, Regexps}, Pos2);
    nomatch -> nomatch
  end;
match(_, {seq, _}, _) ->
  nomatch.


char_test() ->
  {match, [{0, 1}]} = run("a", {char, $a}),
  nomatch = run("b", {char, $a}).


seq_test() ->
  {match, [{0, 2}]} = run("ab", {seq, [{char, $a}, {char, $b}]}).
