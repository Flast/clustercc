-module(node_manager).
-export([get/0, new/1, new_link/1, enter/1, leave/1]).
-import(lists, [reverse/1]).

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

nodes_RR([]) ->
  receive
    {Pid, get} ->
      Pid ! {clusterccd_nodes_pool, self(), undefined},
      Next = [];

    {manage, enter, Node} ->
      Next = [Node],
      common_io:prefixed("enter new node: ~w", Next);
    {manage, leave, _} ->
      common_io:prefixed("cannot leave node"),
      Next = []
  end,
  nodes_RR(Next);

nodes_RR(L = [H | T]) ->
  receive
    {Pid, get} ->
      Pid ! {clusterccd_nodes_pool, self(), H},
      Next = reverse([H | reverse(T)]);

    {manage, enter, Node} ->
      common_io:prefixed("enter new node: ~w", [Node]),
      Next = [Node | L];
    {manage, leave, Node} ->
      common_io:prefixed("leave node: ~w", [Node]),
      Next = lists:delete(Node, L)
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
