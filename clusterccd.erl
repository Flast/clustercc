-module(clusterccd).
-export([main/0]).

prefixed(Format) -> prefixed(Format, []).
prefixed(Format, Data) when length(Format) /= 0 ->
  F = lists:concat(["clusterccd[~w]: ", Format, "~n"]),
  D = [self() | Data],
  io:format(F, D).

main() ->
  true = hostname_validation(),
  prefixed("start server"),
  process_flag(trap_exit, true),
  yes = global:register_name(clusterccd, self()),

  prefixed("node name as ~s", [node()]),

  Manager = node_manager:new_link([]),
  loop(Manager),

  prefixed("terminate").

hostname_validation() ->
  case node() of
    nonode@nohost -> prefixed("hostname validation failed");
    _             -> true
  end.

loop(Manager) when is_pid(Manager) ->
  receive
    {'EXIT', Manager, Why} ->
      prefixed("detect critical error: ~w", Why);
    {'EXIT', Pid, Why} ->
      prefixed("process (~w) was terminated: ~w", [Pid, Why]),
      loop(Manager);

    {node, Pid, join} ->
      prefixed("joining distcc node: ~w", [Pid]),
      loop(Manager);

    {manage, Pid, join} ->
      prefixed("joining management process: ~w", [Pid]),
      loop(Manager);
    {manage, Pid, terminate} ->
      prefixed("receive terminate signal from [~w]", [Pid]),
      Manager ! {manage, Pid, terminate}
  end.
