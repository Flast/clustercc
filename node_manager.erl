-module(node_manager).
-export([get/0, new/1, new_link/1]).

prefixed(Format, Data) when length(Format) /= 0 ->
  F = lists:concat(["node manager[~w]: ", Format, "~n"]),
  D = [self() | Data],
  io:format(F, D).

unique(List) when is_list(List) ->
  sets:to_list(sets:from_list(List)).

get() ->
  Self = self(),
  try
    global:send(nodes_pool, {Self, get}),
    receive {nodes_pool, _, V} -> V end
  catch
    % no such registered name or pid
    exit:{badarg, {nodes_pool, {Self, get}}} -> undefined
  end.

new(Nodes, Spawn) when is_list(Nodes) ->
  Self = self(),
  Pid = Spawn(
    fun() ->
        Regstat = global:register_name(nodes_pool, self()),
        Self ! {nodes_pool, self(), Regstat},
        Unodes = unique(Nodes),
        nodes_RR(Unodes)
    end),
  receive
    {nodes_pool, Pid, yes} ->
      prefixed("start nodes pool: ~w", [Pid]),
      Pid;
    {nodes_pool, Pid, _} -> {error, register_failed}
  end.

new(Nodes) -> new(Nodes, fun spawn/1).
new_link(Nodes) -> new(Nodes, fun spawn_link/1).

nodes_get(Pid, [H | T]) ->
  Pid ! {nodes_pool, self(), H},
  lists:reverse([H | lists:reverse(T)]);
nodes_get(Pid, []) ->
  Pid ! {nodes_pool, self(), undefined},
  [].

nodes_enter(Nodes, L) ->
  Unodes = unique(Nodes),
  {Exist, New} = lists:partition(fun(X) -> lists:member(X, L) end, Unodes),
  NewL = case New of
    [] -> L;
    _  ->
      prefixed("enter new nodes: ~w", [New]),
      lists:concat([New, L])
  end,

  case Exist of
    [] -> undefined;
    _  -> prefixed("following nodes already entered: ~w", [Exist])
  end,
  NewL.

nodes_leave(Nodes, L) ->
  Unodes = unique(Nodes),
  {Tgt, NotMem} = lists:partition(fun(X) -> lists:member(X, L) end, Unodes),
  NewL = case Tgt of
    [] -> L;
    _  ->
      prefixed("leave nodes: ~w", [Tgt]),
      L -- Tgt
  end,

  case NotMem of
    [] -> undefined;
    _  -> prefixed("following nodes not entered: ~w", [NotMem])
  end,
  NewL.

nodes_RR(L) when is_list(L) ->
  Next = receive
    {Pid, get} -> nodes_get(Pid, L);

    {manage, _, enter, Node} -> nodes_enter(Node, L);
    {manage, _, leave, Node} -> nodes_leave(Node, L);

    {manage, Pid, terminate} ->
      prefixed("terminate node manager: signaled by [~w]", [Pid]),
      exit(normal)
  end,
  nodes_RR(Next).
