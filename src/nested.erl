-module(nested).
-author("vlan").
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").


parse(S) ->
  case nested_file(S) of
    {ok, Result, _} -> {ok, Result};
    {error, Msg, _} -> {error, Msg}
  end.


nested_file(S) ->
  P = funparse:map(
    fun ([Result, _]) -> Result end,
    funparse:seq([
      fun nested/1,
      fun funparse:finished/1
    ])),
  P(S).


nested(S) ->
  P = funparse:map(
    fun ([_, List, _]) -> List end,
    funparse:seq([
      funparse:char($(),
      funparse:many(fun nested/1),
      funparse:char($))])),
  P(S).


parse_test() ->
  ?assertEqual({ok, []}, parse("()")),
  ?assertEqual({ok, [[], [[]]]}, parse("(()(()))")).


not_parse_test() ->
  ?assertEqual({error, "Expected '(', but found 'a'"}, parse("a")),
  ?assertEqual({error, "Expected ')', but found end of file"}, parse("(")),
  ?assertEqual({error, "Expected end of file, but found '('"}, parse("()(")).

