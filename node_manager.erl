-module(node_manager).
-export([get/0, new/1, new_link/1]).

get() ->
  Self = self(),
  try
    global:send(nodes_pool, {Self, get}),
    receive
      {nodes_pool, _, V} -> V
    end
  catch
    % no such registered name or pid
    exit:{badarg, {nodes_pool, {Self, get}}} -> undefined
  end.

new(Nodes, Spawn) when is_list(Nodes) ->
  Pid = Spawn(fun() -> nodes_RR(Nodes) end),
  yes = global:register_name(nodes_pool, Pid),
  common_io:prefixed("start nodes pool: ~w", [Pid]),
  Pid.

new(Nodes) -> new(Nodes, fun spawn/1).
new_link(Nodes) -> new(Nodes, fun spawn_link/1).

nodes_get(Pid, [H | T]) ->
  Pid ! {nodes_pool, self(), H},
  lists:reverse([H | lists:reverse(T)]);
nodes_get(Pid, []) ->
  Pid ! {nodes_pool, self(), undefined},
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

    {manage, _, enter, Node} -> nodes_enter(Node, L);
    {manage, _, leave, Node} -> nodes_leave(Node, L);

    {manage, Pid, terminate} ->
      common_io:prefixed("terminate node manager: signaled by [~w]", [Pid]),
      exit(normal)
  end,
  nodes_RR(Next).
