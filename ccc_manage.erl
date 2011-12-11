-module(ccc_manage).
-export([connect/1, connect/2, stop/0, enter/1, leave/1]).

connect_impl(Type, Node) ->
  pong = net_adm:ping(Node),
  ok   = global:sync(),
  Regs = [clusterccd, nodes_pool],
  Pids = [global:whereis_name(Reg) || Reg <- Regs],
  true = not lists:member(undefined, Pids),

  M    = {Type, self(), join},
  try global:send(clusterccd, M) of
    _ -> true
  catch
    exit:{badarg, {clusterccd, M}} -> false
  end.

connect(Node) -> connect_impl(node, Node).
connect(manage, Node) -> connect_impl(manage, Node).

manage(To, Arg = [F | A]) when is_atom(F), is_list(A) ->
  Self = self(),
  M = list_to_tuple([manage, Self | Arg]),
  try global:send(To, M) of
    _ -> true
  catch
    exit:{badarg, {To, M}} -> false
  end.

stop() ->
  io:format("send terminate signal~n"),
  case manage(clusterccd, [terminate]) of
    true  ->
      io:format("leave manage shell~n"),
      init:stop();
    false -> undefined
  end.

enter(Node) -> manage(nodes_pool, [enter, Node]).
leave(Node) -> manage(nodes_pool, [leave, Node]).
