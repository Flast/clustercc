-module(ccc_manage).
-export([stop/0, enter/1, leave/1]).

trap_exit_RAII(Function) ->
  process_flag(trap_exit, true),
  Function(),
  process_flag(trap_exit, false).

stop() ->
  F = fun() ->
    common_io:prefixed("send terminate signal"),
    clusterccd ! {manage, terminate}
    end,
  trap_exit_RAII(F).

manage(Function, Arg) when is_atom(Function) ->
  try clusterccd_nodes_pool ! {manage, Function, Arg} of
    {manage, Function, Arg} -> true
  catch
    error:badarg -> false
  end.

enter(Node) -> manage(enter, Node).
leave(Node) -> manage(leave, Node).
