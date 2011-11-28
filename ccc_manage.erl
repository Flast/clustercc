-module(ccc_manage).
-export([connect/1, stop/0, enter/1, leave/1]).

connect(Node) ->
  pong = net_adm:ping(Node),
  ok   = global:sync(),
  Regs = [clusterccd, nodes_pool],
  Pids = [global:whereis_name(Reg) || Reg <- Regs],
  not lists:member(undefined, Pids).

trap_exit_RAII(Function) ->
  Prev = process_flag(trap_exit, true),
  Function(),
  true = process_flag(trap_exit, Prev).

stop() ->
  F = fun() ->
    common_io:prefixed("send terminate signal"),
    global:send(clusterccd, {manage, self(), terminate})
    end,
  trap_exit_RAII(F).

manage(Function, Arg) when is_atom(Function) ->
  Self = self(),
  try global:send(nodes_pool, {manage, Self, Function, Arg}) of
    {manage, Self, Function, Arg} -> true
  catch
    error:badarg -> false
  end.

enter(Node) -> manage(enter, Node).
leave(Node) -> manage(leave, Node).
