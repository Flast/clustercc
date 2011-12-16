-module(clusterccd).
-export([main/0]).

prefixed(Format) -> prefixed(Format, []).
prefixed(Format, Data) when length(Format) /= 0 ->
  F = lists:concat(["clusterccd[~w]: ", Format, "~n"]),
  D = [self() | Data],
  io:format(F, D).

workers_create() ->
  ok = pg2:create(workers).

workers_delete() ->
  ok = pg2:delete(workers).

workers_join(Pid) when is_pid(Pid) ->
  ok = pg2:join(workers, Pid).

workers_members() ->
  case pg2:get_members(workers) of
    {error, {no_such_group, workers}} -> [];
    Pids                              -> Pids
  end.

main() ->
  true = hostname_validation(),
  prefixed("start server"),
  process_flag(trap_exit, true),
  true = register(clusterccd, self()),
  yes  = global:register_name(clusterccd, self()),
  ok   = workers_create(),

  prefixed("node name as ~s", [node()]),

  ok     = start_application(),
  Daemon = start_ssh_daemon(any, 22),

  Manager = node_manager:new_link([]),
  register(nodes_pool, Manager),
  loop(Manager),

  signal_terminate(Daemon),

  prefixed("joining all sessions ..."),
  join_all_processes(Manager, workers_members()),
  prefixed("joined"),

  % Stop all applications after all connected sessions are finished.
  ssh:stop_daemon(Daemon),
  application:stop(ssh),
  application:stop(crypto),

  ok = workers_delete(),
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

  prefixed("start application"),
  ok = StartApplication(crypto),
  StartApplication(ssh).

start_ssh_daemon(Addr, Port)
when is_integer(Port), 0 =< Port, Port < 65536 ->
  prefixed("start ssh daemon"),
  {ok, Daemon} = ssh:daemon(Addr, Port, [{ssh_cli, {worker, []}}]),
  Daemon.

signal_terminate(Daemon) ->
  ssh:stop_listener(Daemon),
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
      prefixed("receive terminate signal from [~w]", [Pid]);

    {node, _, Npid} ->
      link(Npid),
      ok = workers_join(Npid),
      prefixed("spawn new node: ~w", [Npid]),
      loop(Manager)
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
