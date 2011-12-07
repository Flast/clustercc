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

trap_exit_RAII(Function) ->
  Prev = process_flag(trap_exit, true),
  R    = Function(),
  true = process_flag(trap_exit, Prev),
  R.

manage(To, Arg = [F | A]) when is_atom(F), is_list(A) ->
  Self = self(),
  M = list_to_tuple([manage, Self | Arg]),
  try global:send(To, M) of
    _ -> true
  catch
    exit:{badarg, {To, M}} -> false
  end.

stop() ->
  F = fun() ->
    common_io:prefixed("send terminate signal"),
    manage(clusterccd, [terminate])
    end,
  trap_exit_RAII(F).

enter(Node) -> manage(nodes_pool, [enter, Node]).
leave(Node) -> manage(nodes_pool, [leave, Node]).
