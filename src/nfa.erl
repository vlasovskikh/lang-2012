-module(nfa).
-author("vlan").
-export([from_regexp/1, run/2]).


from_regexp(Regexp) -> from_regexp(Regexp, graph:new()).


run(S, {Start, Stop, Graph}) ->
  States = epsilon_closure(sets:from_list([Start]), Graph),
  run(S, States, Stop, Graph).


from_regexp(epsilon, G) ->
  new_nfa(epsilon, G);
from_regexp({char, C}, G) ->
  new_nfa({char, C}, G);
from_regexp({seq, Regexps}, G) ->
  lists:foldl(fun (Regexp, {Start1, Stop1, G1}) ->
                {Start2, Stop2, G2} = from_regexp(Regexp, G1),
                G3 = graph:add_edge(Stop1, Start2, epsilon, G2),
                {Start1, Stop2, G3}
              end,
              new_nfa(epsilon, G),
              Regexps);
from_regexp({star, Regexp}, G) ->
  {Start1, Stop1, G2} = from_regexp(Regexp, G),
  {Start2, Stop2, G3} = new_nfa(epsilon, G2),
  G4 = graph:add_edge(Start2, Start1, epsilon, G3),
  G5 = graph:add_edge(Stop1, Stop2, epsilon, G4),
  G6 = graph:add_edge(Stop1, Start1, epsilon, G5),
  {Start2, Stop2, G6}.


new_nfa(Label, G) ->
  {S1, G2} = graph:add_node(G),
  {S2, G3} = graph:add_node(G2),
  G4 = graph:add_edge(S1, S2, Label, G3),
  {S1, S2, G4}.


fixpoint(F, X) ->
  Y = F(X),
  if X =:= Y -> X;
     true -> fixpoint(F, Y)
  end.


epsilon_set(States, Graph) ->
  sets:fold(
    fun (State, Acc) ->
      Edges = graph:edges_from(State, Graph),
      EpsilonEdges = lists:filter(
        fun (Edge) ->
          graph:edge_label(Edge) =:= epsilon
        end,
        Edges),
      Targets = lists:map(fun graph:edge_target/1, EpsilonEdges),
      sets:union(Acc, sets:from_list(Targets))
    end,
    sets:new(),
    States).


epsilon_closure(States, Graph) ->
  fixpoint(
    fun (Set) ->
      sets:union(Set, epsilon_set(Set, Graph))
    end,
    States).


run([], States, Stop, _) ->
  sets:is_element(Stop, States);
run([C|Cs], States, Stop, Graph) ->
  Moved = sets:fold(
    fun (State, Acc) ->
      NextStates = move(C, State, Graph),
      sets:union(Acc, NextStates)
    end,
    sets:new(),
    States),
  EpsilonClosed = epsilon_closure(Moved, Graph),
  % DEBUG:
  %io:format("~p ~p-> ~p~n",
  %          [sets:to_list(States), C, sets:to_list(EpsilonClosed)]),
  run(Cs, EpsilonClosed, Stop, Graph).


move(C, State, Graph) ->
  Edges = graph:edges_from(State, Graph),
  ActiveEdges = lists:filter(
    fun (Edge) ->
      match(C, graph:edge_label(Edge))
    end,
    Edges),
  NextStates = lists:map(fun graph:edge_target/1, ActiveEdges),
  sets:from_list(NextStates).


match(_, epsilon) ->
  false;
match(C1, {char, C2}) ->
  C1 =:= C2.