-module(graph).
-author("vlan").
-export([
    new/0, add_node/1, add_edge/4, edges_from/2, edge_target/1,
    edge_label/1, to_dot/1]).
-include_lib("eunit/include/eunit.hrl").


new() ->
  {graph, 0, dict:new()}.


add_node({graph, N, Edges}) ->
  Edges2 = dict:store(N, [], Edges),
  {{node, N}, {graph, N + 1, Edges2}}.


add_edge({node, Src}, {node, Dst}, Label, {graph, N, Edges}) ->
  % TODO: Don't add an edge if it already exists
  {graph, N, dict:append(Src, {Dst, Label}, Edges)}.


edges_from({node, Src}, {graph, _, Edges}) ->
  case dict:find(Src, Edges) of
    {ok, Value} ->
      lists:map(fun ({Dst, Label}) -> {edge, Src, Dst, Label} end, Value);
    error ->
      []
  end.


edge_label({edge, _, _, Label}) -> Label.


edge_target({edge, _, N, _}) -> {node, N}.


to_dot({graph, _, Edges}) ->
  Lines = dict:fold(
    fun (Src, Value, Xs) ->
      NewXs = lists:foldl(
        fun ({Dst, Label}, Ys) ->
          Y = io_lib:format("n~p -> n~p [label=\"~p\"]", [Src, Dst, Label]),
          [Y|Ys]
        end,
        [],
        Value),
      NewXs ++ Xs
    end,
    [],
    Edges),
  "digraph {\n" ++ string:join(Lines, "\n") ++ "\n}".


graph_test() ->
  G1 = new(),
  {N1, G2} = add_node(G1),
  {N2, G3} = add_node(G2),
  G4 = add_edge(N1, N2, hello, G3),
  FromN1 = edges_from(N1, G4),
  ?assert(length(FromN1) =:= 1),
  N1ToN2 = lists:nth(1, FromN1),
  ?assert(edge_label(N1ToN2) =:= hello),
  ?assert(edge_target(N1ToN2) =:= N2),
  FromN2 = edges_from(N2, G4),
  ?assert(length(FromN2) =:= 0).

