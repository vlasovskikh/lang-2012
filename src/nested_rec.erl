-module(nested_rec).
-author("vlan").
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

% Nested regexp? No!
%
% \((\(|\))*\)
% {({|})*}


% Nested EBNF
%
% nested = "(" , { nested } , ")"


% (()(()))
% Example0 = {nested, [empty, {nested, [empty]}]}.
% Example1 = [[], [[]]].


parse(S) ->
  case parse_nested(S) of
    {ok, Result, _} -> {ok, Result};
    {error, Msg} -> {error, Msg}
  end.


parse_nested([$(|S]) ->
  {List, S2} = parse_sequence(S),
  case S2 of
    [$)|S3] -> {ok, List, S3};
    _ -> ") expected"
  end;
parse_nested(_) ->
  {error, "( expected"}.


parse_sequence(S) ->
  case parse_nested(S) of
    {ok, Result, S2} ->
      {List, S3} = parse_sequence(S2),
      {[Result|List], S3};
    {error, _} ->
      {[], S}
  end.


parse_test() ->
  {ok, []} = parse("()"),
  {ok, [[], [[]]]} = parse("(()(()))").

