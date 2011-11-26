-module(node_manager).
-export([get/0, new/1, new_link/1, enter/1, leave/1]).

get() ->
  try
    clusterccd_nodes_pool ! {self(), get},
    receive
      {clusterccd_nodes_pool, _, V} -> V
    end
  catch
    % no such registered name or pid
    error:badarg -> undefined
  end.

new(Nodes, Spawn) when is_list(Nodes) ->
  Pid = Spawn(fun() -> nodes_RR(Nodes) end),
  true = register(clusterccd_nodes_pool, Pid),
  common_io:prefixed("start nodes pool: ~w", [Pid]),
  Pid.

new(Nodes) -> new(Nodes, fun spawn/1).
new_link(Nodes) -> new(Nodes, fun spawn_link/1).

nodes_get(Pid, [H | T]) ->
  Pid ! {clusterccd_nodes_pool, self(), H},
  lists:reverse([H | lists:reverse(T)]);
nodes_get(Pid, []) ->
  Pid ! {clusterccd_nodes_pool, self(), undefined},
  [].

nodes_enter(Node, L) ->
  case lists:member(Node, L) of
    true ->
      common_io:prefixed("such node already entered"),
      L;
    false ->
      common_io:prefixed("enter new node: ~w", [Node]),
      [Node | L]
  end.

nodes_leave(Node, L) ->
  case lists:member(Node, L) of
    true ->
      common_io:prefixed("leave node: ~w", [Node]),
      lists:delete(Node, L);
    false ->
      common_io:prefixed("cannot leave such node"),
      L
  end.

nodes_RR(L) when is_list(L) ->
  Next = receive
    {Pid, get} -> nodes_get(Pid, L);

    {manage, enter, Node} -> nodes_enter(Node, L);
    {manage, leave, Node} -> nodes_leave(Node, L)
  end,
  nodes_RR(Next).

manage(Function, Arg) when is_atom(Function) ->
  try clusterccd_nodes_pool ! {manage, Function, Arg} of
    {manage, Function, Arg} -> true
  catch
    error:badarg -> false
  end.

enter(Node) -> manage(enter, Node).
leave(Node) -> manage(leave, Node).
