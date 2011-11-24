-module(node_manager).
-export([get/0, new/1, enter/1, leave/1]).
-import(lists, [reverse/1]).

get() ->
  clusterccd_nodes_pool ! {self(), get},
  receive
    {clusterccd_nodes_pool, _, V} -> V
  end.

new(Nodes) ->
  Monitor = spawn_link(fun() -> monitor(Nodes) end),
  Monitor ! {manage, self(), act},
  receive Pid -> Pid end.

monitor(Nodes) when is_list(Nodes) ->
  common_io:prefixed("start nodes monitor"),
  process_flag(trap_exit, true),
  Pid = spawn_link(fun() -> nodes_RR(Nodes) end),
  true = register(clusterccd_nodes_pool, Pid),
  common_io:prefixed("start nodes pool: ~w", [Pid]),
  receive
    {manage, Caller, act} -> Caller ! Pid
  end,
  receive
    {'EXIT', _, Why} -> Why
  end,
  common_io:prefixed("terminate nodes monitor"),
  exit(Why).

manage(Function, Arg) when is_atom(Function) ->
  clusterccd_nodes_pool ! {manage, Function, Arg}.

enter(Node) -> manage(enter, Node).
leave(Node) -> manage(leave, Node).

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
