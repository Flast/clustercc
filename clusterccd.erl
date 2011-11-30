-module(clusterccd).
-export([main/0]).

main() ->
  true = hostname_validation(),
  common_io:prefixed("start server"),
  process_flag(trap_exit, true),
  yes = global:register_name(clusterccd, self()),

  common_io:prefixed("node name as ~s", [node()]),

  Manager = node_manager:new_link([]),
  loop(Manager),

  common_io:prefixed("terminate").

hostname_validation() ->
  case
    case node() of
      nonode@nohost -> false;
      _             -> true
    end
  of
    true  -> true;
    false -> common_io:prefixed("hostname validation failed")
  end.

loop(Manager) when is_pid(Manager) ->
  EXIT = 'EXIT',
  receive
    {EXIT, Manager, Why} ->
      common_io:prefixed("detect critical error: ~w", Why);
    {EXIT, Pid, Why} ->
      common_io:prefixed("process (~w) was terminated: ~w", [Pid, Why]),
      loop(Manager);

    {manage, Pid, join} ->
      common_io:prefixed("joining management process: ~w", [Pid]),
      loop(Manager);
    {manage, Pid, terminate} ->
      common_io:prefixed("receive terminate signal from [~w]", [Pid]),
      Manager ! {manage, Pid, terminate}
  end.
