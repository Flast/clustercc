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

  ok     = start_application(),
  Daemon = start_ssh_daemon(any, 22),

  Manager = node_manager:new_link([]),
  register(nodes_pool, Manager),
  loop(Manager),

  signal_terminate(Daemon),

  prefixed("joining all sessions ..."),
  join_all_processes(Manager, []),

  % Stop all applications after all connected sessions are finished.
  application:stop(ssh),
  application:stop(crypto),

  prefixed("terminate").

hostname_validation() ->
  case node() of
    nonode@nohost -> prefixed("hostname validation failed");
    _             -> true
  end.

start_application() ->
  Format = "~s application start failed with reason: ~w",

  StartApplication = fun(Name) when is_atom(Name) ->
      case application:start(Name) of
        ok              -> ok;
        {error, Reason} ->
          prefixed(Format, [atom_to_list(Name), Reason]),
          {error, Reason}
      end
  end,

  ok = StartApplication(crypto),
  StartApplication(ssh).

start_ssh_daemon(Addr, Port)
when is_integer(Port), 0 =< Port, Port < 65536 ->
  Shell = fun(User) ->
      io:format("Hi ~s! You've successfully authenticated.~n", [User]),
      io:format("  This server does not provide shell access and close this session.~n~n")
  end,

  Options = [
    {shell, fun(User) -> spawn(fun() -> Shell(User) end) end}
  ],
  {ok, Daemon} = ssh:daemon(Addr, Port, Options),
  Daemon.

signal_terminate(Daemon) ->
  ssh:stop_daemon(Daemon),
  % Send terminate signal
  nodes_pool ! {manage, self(), terminate},
  ok.

loop(Manager) when is_pid(Manager) ->
  receive
    {'EXIT', Manager, Why} ->
      prefixed("detect critical error: ~w", Why);
    {'EXIT', Pid, Why} ->
      prefixed("process (~w) was terminated: ~w", [Pid, Why]),
      loop(Manager);

    {manage, Pid, join} ->
      prefixed("joining management process: ~w", [Pid]),
      loop(Manager);
    {manage, Pid, terminate} ->
      prefixed("receive terminate signal from [~w]", [Pid])
  end.

join_all_processes(Manager, []) ->
  receive
    {'EXIT', Manager, Why} ->
      Prefix = "joining clustercc node manager",
      case Why of
        normal -> prefixed("~s: ~w", [Prefix, Manager]);
        _      -> prefixed("~s(~w) with critical error: ~w", [Prefix, Manager, Why])
      end,
      terminated
  end;
join_all_processes(Manager, [Node | Ntail]) ->
  receive
    {'EXIT', Node, Why} ->
      prefixed("joining node(~w) with status: ~w", [Node, Why]),
      join_all_processes(Manager, Ntail)
  end.
